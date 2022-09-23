# haskell-dapr

![Haskell loves Dapr](./docs/assets/haskell-dapr-400x200.jpg)

Bring [dapr](https://dapr.io/) to Haskell world.

## Local development

### Install the Dapr CLI

See [Install the Dapr CLI](https://docs.dapr.io/getting-started/install-dapr-cli/)

### Install protoc

Follow the document [Installing protoc](https://github.com/google/proto-lens/blob/master/docs/installing-protoc.md)

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
