export enum ActionType {
  UpdateLoading = 'loading-update-loading',
  DoneLoading = 'loading-done-loading',
}

export interface LoadingState {
  loading: boolean;
  message: string;
}

export class UpdateLoadingAction {
  public readonly type = ActionType.UpdateLoading;
  constructor(public payload: LoadingState['message']) {}
}

export class DoneLoadingAction {
  public readonly type = ActionType.DoneLoading;
}

type Action = UpdateLoadingAction | DoneLoadingAction;

export const DEFAULT_STATE: LoadingState = {
  loading: false,
  message: '',
};

const loading = (state = DEFAULT_STATE, action: Action): LoadingState => {
  switch (action.type) {
    case ActionType.UpdateLoading:
      return {
        ...state,
        loading: true,
        message: action.payload,
      };
    case ActionType.DoneLoading:
      return {
        ...state,
        ...DEFAULT_STATE,
      };
    default:
      return state;
  }
};

export default loading;
