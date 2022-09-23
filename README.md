# haskell-dapr

![Haskell loves Dapr](./docs/assets/haskell-dapr-400x200.jpg)

Bring [dapr](https://dapr.io/) to Haskell world.

## Local development

### Setup Haskell development environment

See [How to get started with Haskell in 2022 (the straightforward way)](https://wasp-lang.dev/blog/2022/09/02/how-to-get-started-with-haskell-in-2022)

### Install Docker

Windows/MacOS: [Docker desktop](https://docs.docker.com/desktop/)

Linux: [Docker engine](https://docs.docker.com/engine/install/)

### Install the Dapr CLI

See [Install the Dapr CLI](https://docs.dapr.io/getting-started/install-dapr-cli/)

### Install protoc

Follow the document [Installing protoc](https://github.com/google/proto-lens/blob/master/docs/installing-protoc.md)

The default `protoc` installed by above scripts requires `sudo` permission, you can update the permission by

```bash
chmod 755 /usr/local/bin/protoc
```

### Clone code

```bash
git clone https://github.com/nodew/haskell-dapr.git

cd haskell-dapr
```

### Run tests

#### Setup dependencies

```bash
./scripts/test-init.sh
```

#### Init dapr

```bash
dapr init

dapr run --app-id haskell-dapr --dapr-http-port 3500 --components-path ./components
```

#### Execute tests

```bash
stack test
```

## TODO

- [ ] Add more tests
- [ ] Add Server module
- [ ] Create a sample app
- [ ] GRPC integration
