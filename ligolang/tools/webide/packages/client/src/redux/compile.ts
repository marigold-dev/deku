import { ActionType as ExamplesActionType, ChangeSelectedAction as ChangeSelectedExampleAction } from './examples';

export enum MichelsonFormat {
  Text = 'text',
  Json = 'json'
}

export enum ActionType {
  ChangeEntrypoint = 'compile-change-entrypoint',
  ChangeMichelsonFormat = 'compile-change-michelson-format'
}

export interface CompileState {
  entrypoint: string;
  michelsonFormat?: MichelsonFormat;
}

export class ChangeEntrypointAction {
  public readonly type = ActionType.ChangeEntrypoint;
  constructor(public payload: CompileState['entrypoint']) {}
}

export class ChangeMichelsonFormatAction {
  public readonly type = ActionType.ChangeMichelsonFormat;
  constructor(public payload: CompileState['michelsonFormat']) {}
}

type Action =
  | ChangeEntrypointAction
  | ChangeMichelsonFormatAction
  | ChangeSelectedExampleAction;

const DEFAULT_STATE: CompileState = {
  entrypoint: '',
  michelsonFormat: MichelsonFormat.Text
};

const compile = (state = DEFAULT_STATE, action: Action): CompileState => {
  switch (action.type) {
    case ExamplesActionType.ChangeSelected:
      return {
        ...state,
        ...(!action.payload ? DEFAULT_STATE : action.payload.compile)
      };
    case ActionType.ChangeEntrypoint:
      return {
        ...state,
        entrypoint: action.payload
      };
    case ActionType.ChangeMichelsonFormat:
      return {
        ...state,
        michelsonFormat: action.payload
      };
    default:
      return state;
  }
};

export default compile