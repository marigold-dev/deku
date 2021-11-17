export enum Language {
  PascaLigo = 'pascaligo',
  CameLigo = 'cameligo',
  ReasonLIGO = 'reasonligo',
}

export enum CommandType {
  Compile = 'compile',
  CompileFunction = 'compile-function',
  DryRun = 'dry-run',
  EvaluateValue = 'evaluate-expr',
  EvaluateFunction = 'evaluate-function',
  Deploy = 'deploy',
  GenerateDeployScript = 'generate-deploy-script',
}

export enum Tool {
  TezosClient = 'tezos-client',
}
