set shell := ["pwsh", "-NoLogo", "-NoProfile", "-Command"]

bootstrap:
    mise run bootstrap

build:
    mise run build

test:
    mise run test

check:
    mise run check

