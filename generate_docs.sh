#!/usr/bin/env bash

set -euxo pipefail

ENVIRONMENT="${ENVIRONMENT:-development}"

([[ -d ./public ]] && rm -rf ./public) || echo "Skipping directory creation..."
([[ -d "$HOME/.org-timestamps" ]] && rm -rf "$HOME/.org-timestamps") || echo "Skipping..."

echo "Publishing... with current Emacs configurations."
ENVIRONMENT=$ENVIRONMENT \
    emacs --batch --load publish.el --funcall org-publish-all
