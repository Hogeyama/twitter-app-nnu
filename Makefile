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

aws-local-test:
	make clean-local-aws-env
	make start-local-dynamodb
	cabal run aws-local-test || ( make clean-local-aws-env && exit 1 )
	make clean-local-aws-env

clean:
	cabal clean

################################################################################
# docker
################################################################################

# Build a docker image.
docker-build:
	docker build -t name-update-2434 .

docker-run:
	docker run --interactive --tty --rm --network host name-update-2434

################################################################################
# Local AWS
################################################################################

# exported for docker-compose
export DOCKER_NETWORK_NAME := nnu_aws-local

# should equals to ./aws/local/sqs/custom.conf#rest-sqs.bind-port
export LOCAL_SQS_PORT         := 9324
export LOCAL_DYNAMODB_PORT    := 8000
export LOCAL_API_GATEWAY_PORT := 3000

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
