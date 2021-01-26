.PHONY: build clean docker-build ghci heroku-release hlint lint run watch
all: build

ELMJS=public/elm.js

################################################################################
# local
################################################################################

run: build
	cabal v2-run

# Build this package.
build: haskell elm

haskell:
	cabal v2-build

elm:
	elm make src/elm/Main.elm --output $(ELMJS)

clean:
	cabal clean
	# rm $(ELMJS)

# Perform linting with hlint.
hlint: lint
lint:
	hlint src/

################################################################################
# docker
################################################################################

# Build a docker image.
docker-build:
	docker build -t name-update-2434 .

docker-run:
	docker run --interactive --tty --rm --network host name-update-2434

################################################################################
# heroku
################################################################################

# Push the current version of the app to heroku.
heroku-release:
	heroku container:push web
	heroku container:release web

heroku-restart:
	heroku ps:restart

heroku-logs:
	heroku logs

heroku-psql:
	psql $(shell heroku config:get DATABASE_URL)

################################################################################
# アレ
################################################################################

update: build docker-build heroku-release

heroku-ping:
	curl --request GET \
    --header 'Content-Type: application/json' \
    'https://name-update-2434.herokuapp.com/ping'

heroku-get:
	curl --request GET \
    --header 'Content-Type: application/json' \
    'https://name-update-2434.herokuapp.com/nijisanji' \
    | jq .
	curl --request GET \
    --header 'Content-Type: application/json' \
    'https://name-update-2434.herokuapp.com/SEEDs' \
    | jq .
	curl --request GET \
    --header 'Content-Type: application/json' \
    'https://name-update-2434.herokuapp.com/Gamers' \
    | jq .
	curl --request GET \
    --header 'Content-Type: application/json' \
    'https://name-update-2434.herokuapp.com/Since2019' \
    | jq .

local-get:
	curl --request GET \
    --header 'Content-Type: application/json' \
	'localhost:3000/nijisanji' \
    | jq .
	curl --request GET \
    --header 'Content-Type: application/json' \
	'localhost:3000/SEEDs' \
    | jq .
	curl --request GET \
    --header 'Content-Type: application/json' \
	'localhost:3000/Gamers' \
    | jq .
	curl --request GET \
    --header 'Content-Type: application/json' \
	'localhost:3000/Since2019' \
    | jq .

