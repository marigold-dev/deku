export enum ActionType {
  ChangeShareLink = 'share-change-link',
  GetSharedFile = 'get-shared-file',
}

export interface ShareState {
  link: string;
  file: string;
  loaded: boolean;
}

export class ChangeShareLinkAction {
  public readonly type = ActionType.ChangeShareLink;
  constructor(public payload: ShareState['link']) {}
}

const DEFAULT_STATE: ShareState = {
  link: '',
  file: '',
  loaded: false,
};

const share = (state, action: any): ShareState => {
  if (!state) {
    state = DEFAULT_STATE;
  }
  switch (action.type) {
    case ActionType.ChangeShareLink:
      return {
        ...state,
        link: action.payload,
      };
    case ActionType.GetSharedFile:
      return {
        ...state,
        file: action.value,
        loaded: true,
      };
    default:
      return { ...state };
  }
};

export default share;
