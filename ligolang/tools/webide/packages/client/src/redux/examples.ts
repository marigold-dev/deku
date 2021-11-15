import { ExampleState } from './example';

export enum ActionType {
  ChangeSelected = 'examples-change-selected',
  ClearSelected = 'examples-clear-selected',
  SetDefaultList = 'set-default-examples',
}

export interface ExampleItem {
  id: string;
  name: string;
}

export interface ExamplesState {
  selected: ExampleState | null;
  list?: ExampleItem[];
}

// export class SetDefaultList {
//   public readonly type = ActionType.SetDefaultList;
//   constructor(public payload: ExamplesState['list']) {}
// }

export class ChangeSelectedAction {
  public readonly type = ActionType.ChangeSelected;
  constructor(public payload: ExamplesState['selected']) {}
}

export class ClearSelectedAction {
  public readonly type = ActionType.ClearSelected;
}

type Action = ChangeSelectedAction | ClearSelectedAction;

const DEFAULT_STATE = {
  selected: null,
  list: [],
};

const examples = (state = DEFAULT_STATE, action: any): ExamplesState => {
  switch (action.type) {
    case ActionType.ChangeSelected:
      return {
        ...state,
        selected: action.payload,
      };
    case ActionType.ClearSelected:
      return {
        ...state,
        selected: null,
      };
    case ActionType.SetDefaultList:
      return { ...state, list: action.value };
    default:
      return {
        ...state,
      };
  }
};

export default examples;
