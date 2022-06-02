.PHONY: build clean docker-build ghci heroku-release hlint lint run watch
all: build

ELMJS=public/elm.js

################################################################################
# local
################################################################################

run: build
	cabal v2-run

build:
	cabal v2-build

unit-test:
	cabal run unit
	cabal run doctests

aws-local-test: export NNU_USE_LOCAL_AWS=1
aws-local-test: export NNU_TABLE_NAME=NNU
aws-local-test: export NNU_LOCAL_DYNAMODB_PORT=${LOCAL_DYNAMODB_PORT}
aws-local-test:
	make clean-local-aws-env
	make start-local-dynamodb
	cabal run aws-local-test || ( make clean-local-aws-env && exit 1 )
	make clean-local-aws-env

clean:
	cabal clean
	rm -rf ./build

################################################################################
# docker
################################################################################

PUBLIC_ECR_URL  := public.ecr.aws/q3v2w3q5
IMAGE_REPO_NAME := hogeyama/nnu

export DOCKER_BUILDKIT := 1

docker-ecr-login:
	# aws ecr-public get-login-password --region us-east-1 \
	# 	| docker login --username AWS --password-stdin "${PUBLIC_ECR_URL}"

docker-build-depimage: docker-ecr-login
	docker build \
		--cache-from "${PUBLIC_ECR_URL}/${IMAGE_REPO_NAME}:dependency" \
		--build-arg BUILDKIT_INLINE_CACHE=1 \
		--tag "${IMAGE_REPO_NAME}:dependency" \
		--target dependency \
		.

docker-build: docker-build-depimage
	$(eval GIT_BRANCH   := $(shell git symbolic-ref HEAD --short))
	$(eval GIT_REVISION := $(shell git rev-parse HEAD))
	docker build \
		--cache-from "${IMAGE_REPO_NAME}:dependency" \
		--build-arg BUILDKIT_INLINE_CACHE=1 \
		--build-arg GIT_REVISION="${GIT_BRANCH}@${GIT_REVISION}" \
		--tag "${IMAGE_REPO_NAME}" \
		.

docker-update-depimage: docker-build-depimage
	docker tag "${IMAGE_REPO_NAME}:dependency" "${PUBLIC_ECR_URL}/${IMAGE_REPO_NAME}:dependency"
	docker push "${PUBLIC_ECR_URL}/${IMAGE_REPO_NAME}:dependency"

################################################################################
# Local AWS
################################################################################

# exported for docker-compose
export DOCKER_NETWORK_NAME := nnu_aws-local
export LOCAL_DYNAMODB_PORT := 8000

# clean
#######

clean-local-aws-env: remove-docker-network
	cd ./aws/local && docker-compose down

# network
#########

create-docker-network:
	@if docker network ls | grep ${DOCKER_NETWORK_NAME} > /dev/null; then \
		echo 'network ${DOCKER_NETWORK_NAME} already exists'; \
	else \
		echo '[creating network ${DOCKER_NETWORK_NAME}]'; \
		docker network create nnu_aws-local; \
	fi

remove-docker-network: stop-local-dynamodb
	@if docker network ls | grep ${DOCKER_NETWORK_NAME} > /dev/null; then \
		echo '[deleting network]'; \
	docker network rm nnu_aws-local; \
		else \
		echo "network ${DOCKER_NETWORK_NAME} does not exists"; \
	fi

# DynamoDB
##########

start-local-dynamodb: create-docker-network
	@if cd ./aws/local && docker-compose top | grep -i dynamodb > /dev/null; then \
		echo "dynamodb-local is up-to-date"; \
	else \
		docker-compose up -d dynamodb; \
		./dynamodb/initialize.bash > /dev/null; \
	fi

stop-local-dynamodb:
	cd ./aws/local && docker-compose stop dynamodb

################################################################################
# AWS
################################################################################

get-secret-parameters:
	aws ssm get-parameters-by-path --with-decryption --path '/NijisanjiNameUpdate' \
		| jq '.Parameters[] | { name: .Name, value: .Value }'
