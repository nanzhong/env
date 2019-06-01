IMAGE_NAME ?= nanzhong/workstation
COMMIT ?= $(shell git rev-parse HEAD)

.PHONY: all

all: docker-push

docker-build:
	docker build -t $(IMAGE_NAME):$(COMMIT) --no-cache .
	docker tag $(IMAGE_NAME):$(COMMIT) $(IMAGE_NAME):latest

docker-push: docker-build
	docker push $(IMAGE_NAME):$(COMMIT)
	docker push $(IMAGE_NAME):latest
