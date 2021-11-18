import {
  ActionType as ExamplesActionType,
  ChangeSelectedAction as ChangeSelectedExampleAction,
} from './examples';
import { Language } from './types';
import {} from './actions/editor';

export enum ActionType {
  ChangeLanguage = 'editor-change-language',
  ChangeCode = 'editor-change-code',
  ChangeDirty = 'editor-change-dirty',
  ChangeTitle = 'editor-change-title',
  ChangeCursorPosition = 'editor-change-cursor-position',
  ChangeLastEditedTime = 'editor-last-edited-time',
}

export interface CursorPosition {
  lineNumber: Number;
  column: Number;
}

export interface EditorState {
  language: Language;
  code: string;
  title: string;
  dirty: boolean;
  cursorPosition?: CursorPosition | null;
  lastEditedTime?: Date | null;
}

export class ChangeLanguageAction {
  public readonly type = ActionType.ChangeLanguage;
  constructor(public payload: EditorState['language']) {}
}

export class ChangeCodeAction {
  public readonly type = ActionType.ChangeCode;
  constructor(public payload: EditorState['code']) {}
}

export class ChangeDirtyAction {
  public readonly type = ActionType.ChangeDirty;
  constructor(public payload: EditorState['dirty']) {}
}

export class ChangeTitleAction {
  public readonly type = ActionType.ChangeTitle;
  constructor(public payload: EditorState['title']) {}
}

export class ChangeCursorPositionAction {
  public readonly type = ActionType.ChangeCursorPosition;
  constructor(public payload: EditorState['cursorPosition']) {}
}

export class ChangeLastEditedTimeAction {
  public readonly type = ActionType.ChangeLastEditedTime;
  constructor(public payload: EditorState['lastEditedTime']) {}
}

type Action =
  | ChangeCodeAction
  | ChangeLanguageAction
  | ChangeDirtyAction
  | ChangeTitleAction
  | ChangeSelectedExampleAction
  | ChangeCursorPositionAction
  | ChangeLastEditedTimeAction;

const DEFAULT_STATE: EditorState = {
  language: Language.CameLigo,
  code: '',
  title: '',
  dirty: false,
  cursorPosition: null,
  lastEditedTime: null,
};

const editor = (state, action: Action): EditorState => {
  if (!state) {
    state = DEFAULT_STATE;
  }
  switch (action.type) {
    case ExamplesActionType.ChangeSelected:
      return {
        ...state,
        ...action.payload?.editor,
        title: action.payload?.name
          ? action.payload.name
          : action.payload?.editor.title,
      };
    case ActionType.ChangeLanguage:
      return {
        ...state,
        language: action.payload,
      };
    case ActionType.ChangeCode:
      return {
        ...state,
        code: action.payload,
      };
    case ActionType.ChangeDirty:
      return {
        ...state,
        dirty: action.payload,
      };
    case ActionType.ChangeTitle:
      return {
        ...state,
        title: action.payload,
      };
    case ActionType.ChangeCursorPosition:
      return {
        ...state,
        cursorPosition: action.payload,
      };
    case ActionType.ChangeLastEditedTime:
      return {
        ...state,
        lastEditedTime: action.payload,
      };
    default:
      return { ...state };
  }
};

export default editor;
