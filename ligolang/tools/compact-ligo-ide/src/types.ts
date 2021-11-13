export enum Language {
  PascaLigo = 'pascaligo',
  CameLigo = 'cameligo',
  ReasonLigo = 'reasonligo'
};

export interface ShareParams {
  editor: EditorConfig;
  compile: CompileConfig;
  dryRun: DryRunConfig;
  deploy: DeployConfig;
  evaluateFunction: EvaluateFunctionConfig;
  evaluateValue: EvaluateValueConfig;
}

export interface EditorConfig {
  language: Language;
  code: string;
  dirty: boolean;
  title: string;
}

export interface CompileConfig {
  entrypoint: string;
}

export interface DryRunConfig {
  entrypoint: string;
  parameters: string;
  storage: string;
}

export interface DeployConfig {
  entrypoint: string;
  storage: string;
}

export interface EvaluateFunctionConfig {
  entrypoint: string;
  parameters: string;
}

export interface EvaluateValueConfig {
  entrypoint: string;
}

export interface CompactLigoIdeProps {
  editor?: Partial<EditorConfig>;
  compile?: Partial<CompileConfig>;
  dryRun?: Partial<DryRunConfig>;
  deploy?: Partial<DeployConfig>;
  evaluateFunction?: Partial<EvaluateFunctionConfig>;
  evaluateValue?: Partial<EvaluateValueConfig>;
  result?: string;
  webIdeUrl?: string;
  theme?: "dark" | "light";
  children?: string;
}

export const DEFAULT_EDITOR_CONFIG: EditorConfig = {
  code: "",
  language: Language.PascaLigo,
  dirty: true,
  title: ""
};

export const DEFAULT_COMPILE_CONFIG: CompileConfig = {
  entrypoint: ""
};

export const DEFAULT_DRY_RUN_CONFIG: DryRunConfig = {
  entrypoint: "",
  parameters: "",
  storage: ""
};

export const DEFAULT_DEPLOY_CONFIG: DeployConfig = {
  entrypoint: "",
  storage: ""
};

export const DEFAULT_EVALUATE_FUNCTION_CONFIG: EvaluateFunctionConfig = {
  entrypoint: "",
  parameters: ""
};

export const DEFAULT_EVALUATE_VALUE_CONFIG: EvaluateValueConfig = {
  entrypoint: ""
};

export const DEFAULT_SHARE_PARAMS: ShareParams = {
  editor: DEFAULT_EDITOR_CONFIG,
  compile: DEFAULT_COMPILE_CONFIG,
  dryRun: DEFAULT_DRY_RUN_CONFIG,
  deploy: DEFAULT_DEPLOY_CONFIG,
  evaluateFunction: DEFAULT_EVALUATE_FUNCTION_CONFIG,
  evaluateValue: DEFAULT_EVALUATE_VALUE_CONFIG
};

export const DEFAULT_COMPACT_LIGO_IDE_PROPS: (ShareParams & CompactLigoIdeProps) = {
  ...DEFAULT_SHARE_PARAMS,
  result: '',
  webIdeUrl: process.env.NODE_ENV === 'production' ? 'https://ide.ligolang.org' : 'http://localhost:8080',
  theme: 'light'
};
