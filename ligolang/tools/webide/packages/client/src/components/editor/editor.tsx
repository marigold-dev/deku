import { languages } from 'monaco-editor';
import React from 'react';
import { useDispatch, useSelector } from 'react-redux';
import styled from 'styled-components';

import { AppState } from '../../redux/app';
import { ChangeLanguageAction, ChangeTitleAction, EditorState } from '../../redux/editor';
import { Language } from '../../redux/types';
import { Option, Select } from '../form/select';
import { ShareComponent } from '../share';
import { EditableTitleComponent } from './editable-title';
import MonacoComponent from './monaco';

const Container = styled.div`
  flex: 2;
  width: inherit;
  height: 100%;
  order: 2;
`;

const Header = styled.div`
  flex: 1;
  display: flex;
  justify-content: space-between;
  align-items: center;

  background: var(--blue_trans1);
  border: 5px solid rgba(0, 0, 0, 0);
  padding: 0 10px;
`;

const LeftActions = styled.div`
  display: flex;
`;

const StyledEditableTitleComponent = styled(EditableTitleComponent)`
  margin-left: 20px;
`;

const SelectLanguage = styled(Select)`

  &:hover {
    background: var(--blue_trans1);
  }
`;

const CursorPosition = styled.div`
  text-align: right;
  padding: 5px 10px;
  height: 37px;
  background: var(--blue_trans1);
`;


export const EditorComponent = ({editorHeight}) => {
  const dispatch = useDispatch();
  const title = useSelector<AppState, string>(state => state.editor && state.editor.title);
  const language = useSelector<AppState, EditorState['language']>(
    state => state.editor && state.editor.language
  );

  const cursorPosition = useSelector<AppState, EditorState['cursorPosition']>(
    state => state.editor && state.editor.cursorPosition
  );
  
  const getCursorPosition = () => {
    if(cursorPosition) {
      return `Line ${cursorPosition.lineNumber}, Column ${cursorPosition.column}`
    }
  }

  return (
    <Container>
      <Header>
        <LeftActions>
          <ShareComponent></ShareComponent>
          <StyledEditableTitleComponent
            id="editor-title"
            title={title}
            onChanged={value => {
              dispatch({ ...new ChangeTitleAction(value) });
            }}
          ></StyledEditableTitleComponent>
        </LeftActions>
        <LeftActions >
          <SelectLanguage
            id="syntax-select"
            value={language}
            onChange={language => {
              dispatch({ ...new ChangeLanguageAction(language) });
            }}
          >
            <Option value={Language.PascaLigo}>PascaLIGO</Option>
            <Option value={Language.CameLigo}>CameLIGO</Option>
            <Option value={Language.ReasonLIGO}>ReasonLIGO</Option>
          </SelectLanguage>
        </LeftActions>
      </Header>
      <MonacoComponent editorHeight={editorHeight}></MonacoComponent>
      <CursorPosition>{getCursorPosition()}</CursorPosition>
    </Container>
  );
};
