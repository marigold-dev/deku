export enum ActionType {
  SetDefaultList = 'set-default-examples',
}

export interface ListDeclarationState {
  function: string;
}

const DEFAULT_STATE = {
  function: '',
};

const listDeclaration = (
  state = DEFAULT_STATE,
  action: any
): ListDeclarationState => {
  switch (action.type) {
    case ActionType.SetDefaultList:
      return { ...state, function: action.value };
    default:
      return {
        ...state,
      };
  }
};

export default listDeclaration;
