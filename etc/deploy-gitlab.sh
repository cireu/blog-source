#!/usr/bin/env bash

set -euo pipefail

ORIGIN_DIR=${PWD}

git clone --depth=50 \
    "https://citreu:${TOKEN}@gitlab.com/citreu/citreu.gitlab.io" \
    ../gitlab-checkout

pushd ../gitlab-checkout > /dev/null

git config user.name "Zhu Zihao"
git config user.email "all_but_last@163.com"

rm -rf public
cp -r ${ORIGIN_DIR}/docs/.vuepress/dist ./public

git add --all .

if git commit \
       -m "Deploying to Gitlab pages from ${GITHUB_SHA}." \
       --quiet --no-verify; then
    git push origin --quiet
fi

popd > /dev/null
