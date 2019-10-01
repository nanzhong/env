IMAGE_NAME ?= nanzhong/workstation
COMMIT ?= $(shell git rev-parse HEAD)

.PHONY: all
all: build

.PHONY: build
build: emacs tmux workstation

.PHONY: emacs
emacs: GITREF ?= master
emacs: TAG = $(if $(GITREF:master=),$(GITREF),latest)
emacs:
	docker build -t nanzhong/emacs:$(TAG) -f emacs.Dockerfile --build-arg gitref=$(GITREF) .
ifdef PUSH
	docker push nanzhong/emacs:$(TAG)
endif

.PHONY: tmux
tmux: GITREF ?= 2.9a
tmux: TAG = $(if $(GITREF:master=),$(GITREF),latest)
tmux:
	docker build -t nanzhong/tmux:$(TAG) -f tmux.Dockerfile --build-arg gitref=$(GITREF) .
ifdef PUSH
	docker push nanzhong/tmux:$(TAG)
endif

workstation: emacs tmux
	docker build -t $(IMAGE_NAME):$(COMMIT) .
	docker tag $(IMAGE_NAME):$(COMMIT) $(IMAGE_NAME):latest
ifdef PUSH
	docker push $(IMAGE_NAME):$(COMMIT)
	docker push $(IMAGE_NAME):latest
endif
