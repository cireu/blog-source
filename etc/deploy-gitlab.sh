#!/usr/bin/env bash

mkdir ../gitlab-checkout

ORIGIN_DIR=${PWD}

pushd ../gitlab-checkout > /dev/null

git init
git config user.name "Zhu Zihao"
git config user.email "all_but_last@163.com"
git remote set-url "https://citreu:${TOKEN}@gitlab.com/citreu/citreu.gitlab.io"

git fetch --no-recurse-submodules

cp -rf ${ORIGIN_DIR}/.vuepress/dist/* ./

git add --all .

if git commit \
       -m "Deploying to Gitlab pages from ${GITHUB_SHA}." \
       --quiet --no-verify; then
    git push --quiet
fi

popd > /dev/null
