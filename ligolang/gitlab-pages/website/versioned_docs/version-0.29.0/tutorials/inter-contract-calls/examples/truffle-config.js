
// A secret key for demo purposes, generated from seed 'alice'.
// Never use this key for production deployments!
const secret = 'edsk3QoqBuvdamxouPhin7swCvkQNgq4jP5KZPbwWNnwdZpSpJiEbq'

// A syntax to use:
syntax = 'mligo' // CameLIGO
// syntax = 'ligo'   // PascaLIGO
// syntax = 'religo' // ReasonLIGO

module.exports = {
  // see <http://truffleframework.com/docs/advanced/configuration>
  // for more details on how to specify configuration options!
  contracts_directory: `./contracts/${syntax}`,
  networks: {
    development: {
      host: "http://localhost",
      port: 8732,
      network_id: "*",
      secretKey: secret,
      type: "tezos"
    }
  }
}
