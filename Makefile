IMAGE_NAME ?= nanzhong/workstation
COMMIT ?= $(shell git rev-parse HEAD)

.PHONY: build
build:
	docker build $(DOCKER_FLAGS) -t $(IMAGE_NAME):$(COMMIT) .
	docker tag $(IMAGE_NAME):$(COMMIT) $(IMAGE_NAME):latest

.PHONY: push
push:
	docker push $(IMAGE_NAME):$(COMMIT)
	docker push $(IMAGE_NAME):latest
