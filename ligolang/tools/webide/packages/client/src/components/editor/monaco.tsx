import * as monaco from 'monaco-editor';
import React, { useEffect, useRef, useState } from 'react';
import { useDispatch, useStore , connect} from 'react-redux';
import styled from 'styled-components';

import { AppState } from '../../redux/app';
import { ChangeCodeAction, ChangeDirtyAction, ChangeCursorPositionAction, ChangeLastEditedTimeAction } from '../../redux/editor';
import { ChangeOutputAction } from '../../redux/result';
import { ClearSelectedAction } from '../../redux/examples';
import { ListDeclarationAction } from '../../redux/actions/list-declaration'
import {ChangeSelectedAction} from '../../redux/compile-function'
import { CompileFunctionAction } from '../../redux/actions/compile-function';
import {CommandType} from '../../redux/types';

interface TopPaneStyled {
  editorHeight: number;
}
export interface MethodType {
  [x: string]: any;
  declarations: string[];
}

const Container = styled.div<TopPaneStyled>`
  height: ${props => props.editorHeight - 100}px;
  
  /* This font size is used to calcuate code font size */
  font-size: 0.8em;
`;

const MonacoComponent = (props) => {
  const { editorHeight, code, language, getDeclarationList, setCompileFunction, setError } = props
  let containerRef = useRef(null);
  const store = useStore();
  const dispatch = useDispatch();
  const [currentLineText, setCurrentLineText] = useState('')

  const compileFunctionHandler = (currentLine) => {
    if(code==='') {return}
    getDeclarationList(language, code).then((method: MethodType) => {
      method.declarations.forEach(d => {
        if (currentLine.indexOf(d) !== -1) {
          setCompileFunction(d)
          dispatch( new CompileFunctionAction().getAction());
        } else {
          setError('Function not found in the selected line.')
        }
        setCompileFunction('')
      });
    }).catch((error) => {
      setError(error)
    })
  }

    // const onRightClickAction = (model) => {
    //   return {
    //     id: '1',
    //     label: "Compile Expression",
    //     keybindings: [monaco.KeyMod.CtrlCmd | monaco.KeyCode.F10],
	  //     contextMenuGroupId: 'navigation',
	  //     contextMenuOrder: 2.5,
    //     run: (e) => {
    //       const position = e.getPosition()
    //       const currentLine = model && model.getLineContent(position.lineNumber)
    //       setCurrentLineText(currentLine)
    //     }
    //   }
    // }

  useEffect(() => {
    const cleanupFunc: Array<() => void> = [];
    const { editor: editorState } = store.getState();
    const model = monaco.editor.createModel(
      editorState && editorState.code,
      editorState && editorState.language
    );

    monaco.editor.defineTheme('ligoTheme', {
      base: 'vs',
      inherit: true,
      rules: [],
      colors: {
        'editor.background': '#eff7ff',
        'editor.lineHighlightBackground': '#cee3ff',
        'editorLineNumber.foreground': '#888'
      }
    });

    monaco.editor.setTheme('ligoTheme');

    const htmlElement = (containerRef.current as unknown) as HTMLElement;
    const fontSize = window
      .getComputedStyle(htmlElement, null)
      .getPropertyValue('font-size');

    const editor = monaco.editor
    .create(htmlElement, {
      fontSize: parseFloat(fontSize),
      model: model,
      automaticLayout: true,
      minimap: {
        enabled: false
      }
    })

    // const m = editor.getModel()
    // editor.addAction(onRightClickAction(m))

    let shouldDispatchCodeChangedAction = true;

    editor.onDidChangeCursorPosition (() => {
      dispatch({ ...new ChangeCursorPositionAction(editor.getPosition()) });
    })

    const { dispose } = editor.onDidChangeModelContent(() => {
      if (shouldDispatchCodeChangedAction) {
        dispatch({ ...new ChangeCodeAction(editor.getValue()) });
        dispatch({ ...new ChangeDirtyAction(true) });
        dispatch({ ...new ChangeLastEditedTimeAction(new Date()) });
        dispatch({ ...new ClearSelectedAction() });
      }
    });

    cleanupFunc.push(dispose);

    cleanupFunc.push(
      store.subscribe(() => {
        const { editor: editorState }: AppState = store.getState();

        if ( editorState && editorState.code !== editor.getValue()) {
          shouldDispatchCodeChangedAction = false;
          editor.setValue(editorState.code);
          shouldDispatchCodeChangedAction = true;
        }

        if (editorState && editorState.language !== model.getModeId()) {
          if (['reasonligo', 'jsligo'].includes(editorState.language)) {
            monaco.editor.setModelLanguage(model, 'javascript');
          } else {
            monaco.editor.setModelLanguage(model, editorState.language);
          }
        }
      })
    );

    return function cleanUp() {
      cleanupFunc.forEach(f => f());
    };
  }, []);

  useEffect(() => { compileFunctionHandler(currentLineText) }, [currentLineText]);


  return (
  <Container id="editor" ref={containerRef} editorHeight={editorHeight}></Container>
  )};

const mapStateToProps = state => {
  const { editor } = state
  return { 
    code : editor.code,
    language: editor.language
   }
}

const mapDispatchToProps = dispatch => {
  return({
    getDeclarationList: (syntax, code)  => dispatch(ListDeclarationAction(syntax, code)),
    setCompileFunction: (functionName)  => dispatch({...new ChangeSelectedAction(functionName)}),
    setError: (errorMessage) => dispatch({...new ChangeOutputAction(`Error: ${errorMessage}`,CommandType.CompileFunction ,true),})
  })
}

export default connect(mapStateToProps, mapDispatchToProps)(MonacoComponent)