#!/usr/bin/env bash
set -eu
BASE_DIR=$(dirname "$(realpath "$0")")

REGION=ap-northeast-1
ENDPOINT=http://localhost:8000
SCHEMA="file://$BASE_DIR/schema/dynamodb-nnu-develop.yml"
aws dynamodb \
  --region "$REGION" \
  --endpoint-url "$ENDPOINT" \
  create-table \
  --cli-input-yaml "$SCHEMA"
