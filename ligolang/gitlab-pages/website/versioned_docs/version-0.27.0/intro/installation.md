---
id: installation
title: Installation
---

There are currently three ways to get started with LIGO. You can choose to use a Docker image, a static Linux binary or to install packages for your Debian Linux distribution.

## Dockerised installation (recommended)
If you've [installed 🐳 Docker](https://docs.docker.com/install/), you can run the latest [LIGO release 0.29.0](./changelog.md):

Linux or OSX:
> `docker run --rm -v "$PWD":"$PWD" -w "$PWD" ligolang/ligo:0.29.0`

Windows:
> `docker run --rm -v "%CD%":/cd -w /cd ligolang/ligo:0.29.0`

Or if you want the development version, replace the version above with `next`.

Or run one of the older versions found on [DockerHub](https://hub.docker.com/r/ligolang/ligo/tags).

## Static Linux binary

The `ligo` executable is statically linked. It should run on most modern Linux distributions.

To use it, get it [here](https://ligolang.org/bin/linux/ligo), make it executable, and you're done!

```zsh
wget https://ligolang.org/bin/linux/ligo
chmod +x ./ligo
```

Optionally, you can put it somewhere in your `PATH` for easy access:

```zsh
sudo cp ./ligo /usr/local/bin
```

## Debian Linux package installation

A `.deb` package containing the static `ligo` executable is also available.
First, download [the package](https://ligolang.org/deb/ligo.deb), and then install using: 

```zsh
sudo apt install ./ligo.deb
```

## Releases

Releases are available at the [releases page of GitLab project](https://gitlab.com/ligolang/ligo/-/releases). All the artifacts are attached there.

If you wish to see the changelog, you can either run `ligo changelog` or go to [this page](https://ligolang.org/docs/next/intro/changelog). It contains links to corresponding releases, should you wish to download the artifacts.
