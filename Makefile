.DEFAULT_GOAL := build

################################################################################
# local
################################################################################

run: build
	cabal v2-run
.PHONY: run

build:
	cabal v2-build
.PHONY: build

test: test-docker test-no-docker
.PHONY: test

test-no-docker:
	cabal run unit
	cabal run doctests
.PHONY: test-no-docker

test-docker: export NNU_USE_LOCAL_AWS=1
test-docker: export NNU_TABLE_NAME=NNU_DEVELOP
test-docker:
	docker compose down
	docker compose up -d dynamodb
	./scripts/setup-local-dynamodb.bash >/dev/null
	cabal run aws-local-test || ( docker compose down && exit 1 )
	docker compose down
.PHONY: test-docker

clean:
	cabal clean
.PHONY: clean

################################################################################
# docker
################################################################################

PUBLIC_ECR_URL  := public.ecr.aws/q3v2w3q5
IMAGE_REPO_NAME := hogeyama/nnu

export DOCKER_BUILDKIT := 1

# Use https://github.com/awslabs/amazon-ecr-credential-helper instead
# docker-ecr-login:
# 	aws ecr-public get-login-password --region us-east-1 \
# 		| docker login --username AWS --password-stdin "${PUBLIC_ECR_URL}"

docker-build:
	docker build \
		--build-arg BUILDKIT_INLINE_CACHE=1 \
		--build-arg GIT_REVISION="${GIT_BRANCH}@${GIT_REVISION}" \
		--tag "${IMAGE_REPO_NAME}" \
		.
.PHONY: docker-build

################################################################################
# AWS
################################################################################

get-secret-parameters:
	aws ssm get-parameters-by-path --with-decryption --path '/NijisanjiNameUpdate' \
		| jq '.Parameters[] | { name: .Name, value: .Value }'
.PHONY: get-secret-parameters
