FROM debian:sid

RUN apt-get update && apt-get -qy upgrade && apt-get -qy install \
    build-essential apt-transport-https ca-certificates curl gnupg2 software-properties-common locales tzdata ispell mysql-client procps \
    libssl-dev libreadline-dev zlib1g-dev \
    libffi-dev \
    mosh tmux fish wget git jq direnv unzip htop dnsutils git-crypt \
    emacs \
    golang ruby python \
    default-jre graphviz plantuml

RUN echo "en_US.UTF-8 UTF-8" > /etc/locale.gen
RUN locale-gen en_US.UTF-8 
ENV LANG=en_US.UTF-8
ENV LC_CTYPE=en_US.UTF-8
ENV LC_ALL=en_US.UTF-8

# for correct colours in tmux
ENV TERM screen-256color

# use buster because no repo exists for sid
RUN add-apt-repository "deb [arch=amd64] https://download.docker.com/linux/debian buster stable"
RUN curl -fsSL https://download.docker.com/linux/debian/gpg | apt-key add

RUN apt-get update && apt-get -y install docker-ce

RUN chsh -s /usr/bin/fish
RUN mkdir -p /root /root/src /root/bin /root/go/bin

RUN curl -sLO https://storage.googleapis.com/kubernetes-release/release/v1.13.3/bin/linux/amd64/kubectl && chmod +x kubectl && mv kubectl /usr/local/bin/kubectl

ENV KEYBASE_ALLOW_ROOT 1
RUN curl -sO https://prerelease.keybase.io/keybase_amd64.deb && apt-get -y install ./keybase_amd64.deb && rm keybase_amd64.deb

RUN git clone https://github.com/nanzhong/dotfiles.git /root/src/dotfiles
RUN ln -s ~/src/dotfiles/.tmux.conf ~/.tmux.conf
RUN ln -s ~/src/dotfiles/.config ~/.config
RUN ln -s ~/src/dotfiles/.emacs.d ~/.emacs.d
RUN ln -s ~/src/dotfiles/.gitconfig ~/.gitconfig
RUN ln -s ~/src/dotfiles/.gitignore ~/.gitignore
RUN cd /root/src/dotfiles && git remote set-url origin git@github.com:nanzhong/dotfiles.git

RUN git clone https://github.com/nanzhong/emacs-nan-theme.git /root/src/emacs-nan-theme
RUN cd /root/src/emacs-nan-theme && git remote set-url origin git@github.com:nanzhong/emacs-nan-theme.git

RUN emacs --batch --load /root/.emacs.d/init.el --eval '(kill-emacs)'

RUN git clone https://github.com/junegunn/fzf /root/.fzf
RUN cd /root/.fzf && git remote set-url origin git@github.com:junegunn/fzf.git
RUN /root/.fzf/install --bin --64 --no-bash --no-zsh --no-fish

# kubectl fish completion until natively support
RUN mkdir -p /root/.config/fish/completions
RUN curl https://raw.githubusercontent.com/evanlucas/fish-kubectl-completions/master/kubectl.fish > ~/.config/fish/completions/kubectl.fish


RUN fish -c "go get -u github.com/sourcegraph/go-langserver"
RUN fish -c "go get -u github.com/mdempsky/gocode"
RUN fish -c "go get -u golang.org/x/tools/cmd/..."
RUN fish -c "go get -u github.com/aybabtme/humanlog/cmd/..."

COPY session-init.sh /bin/session-init
CMD ["/bin/session-init"]

WORKDIR /root
