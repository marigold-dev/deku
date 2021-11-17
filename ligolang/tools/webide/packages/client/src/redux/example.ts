import { CompileState } from './compile';
import { DeployState } from './deploy';
import { DryRunState } from './dry-run';
import { EditorState } from './editor';
import { EvaluateFunctionState } from './evaluate-function';
import { EvaluateValueState } from './evaluate-expr';
import { GenerateDeployScriptState } from './generate-deploy-script';


export interface ExampleState {
  id: string;
  name: string;
  editor: EditorState;
  compile: CompileState;
  dryRun: DryRunState;
  deploy: DeployState;
  evaluateFunction: EvaluateFunctionState;
  evaluateValue: EvaluateValueState;
  generateDeployScript: GenerateDeployScriptState;
}
