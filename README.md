# haskell-dapr

Bring [dapr](https://dapr.io/) to Haskell world.

## Local development

### Install the Dapr CLI

See [Install the Dapr CLI](https://docs.dapr.io/getting-started/install-dapr-cli/)

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
dapr run --app-id haskell-dapr --app-port 3500 --components-path ./test/components
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
- [ ] Simplify the APIs
