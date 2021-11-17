import React, { useEffect, useState } from 'react';
import { connect } from 'react-redux';
import styled from 'styled-components';
import { PushSpinner } from 'react-spinners-kit';
import { Group, Label } from '../form/inputs';
import { Option, Select } from '../form/select';
import { ListDeclarationAction } from '../../redux/actions/list-declaration';
import { ChangeSelectedAction } from '../../redux/compile-function';
import { ChangeOutputAction } from '../../redux/result';
import { CommandType } from '../../redux/types';

const Container = styled.div``;

const SpinnerWrapper = styled.div`
  display: flex;
  justify-content: center;
  align-content: center;
  margin-top: 20%;
`;

const SelectCommand = styled(Select)`
  flex: 2;

  &:hover {
    box-shadow: var(--box-shadow);
  }
`;

export interface MethodType {
  declarations: string[];
}
const CompileFunctionPaneComponent = (props) => {
  const {
    getDeclarationList,
    code,
    setCompileFunction,
    language,
    setError,
    lastEditedTime,
  } = props;
  const [declaration, setDeclaration] = useState<string[]>([]);
  const [functionName, setFunctionName] = useState<string>('');
  const [showSpinner, setShowSpinner] = useState<boolean>(false);
  let timeout;


  useEffect(() => {
    const lastLoadedTime = lastEditedTime
      ? Math.ceil(
          Math.abs(new Date().getTime() - lastEditedTime.getTime()) / 1000
        )
      : null;
    if (showSpinner) {
      clearTimeout(timeout);
    }
    if (lastLoadedTime === 0) {
      if (!showSpinner) {
        timeout = setTimeout(() => {
          setShowSpinner(true);
          getDeclarationList(language, code)
            .then((file: MethodType) => {
              setDeclaration(file.declarations);
              file.declarations && setFunctionName(file.declarations[0]);
              setShowSpinner(false);
            })
            .catch((error) => {
              setError(error);
              setShowSpinner(false);
            });
        }, 1000);
      }
      return;
    } else if (
      (lastLoadedTime === null || lastEditedTime >= 5) &&
      !showSpinner
    ) {
      setShowSpinner(true);
      getDeclarationList(language, code)
        .then((file: MethodType) => {
          setDeclaration(file.declarations);
          file.declarations && setFunctionName(file.declarations[0]);
          file.declarations && setCompileFunction(file.declarations[0]);
          setShowSpinner(false);
        })
        .catch((error) => {
          setError(error);
          setShowSpinner(false);
        });
    }
  }, [lastEditedTime]);

  return (
    <Container>
      {showSpinner && (
        <SpinnerWrapper>
          <PushSpinner size={50} color="#fa6f41" />
        </SpinnerWrapper>
      )}
      {!showSpinner && declaration && declaration.length > 0 && (
        <Group>
          <Label>Select Function to compile</Label>
          <SelectCommand
            id="command-select"
            value={functionName}
            onChange={(fn) => {
              setFunctionName(fn);
              setCompileFunction(fn);
            }}
          >
            {declaration.map((m) => {
              return (
                <Option key={m} value={m}>
                  {m}
                </Option>
              );
            })}
          </SelectCommand>
        </Group>
      )}
    </Container>
  );
};

const mapStateToProps = (state) => {
  const { editor } = state;
  return {
    code: editor.code,
    language: editor.language,
    lastEditedTime: editor.lastEditedTime,
  };
};

const mapDispatchToProps = (dispatch) => {
  return {
    getDeclarationList: (syntax, code) =>
      dispatch(ListDeclarationAction(syntax, code)),
    setCompileFunction: (functionName) =>
      dispatch({ ...new ChangeSelectedAction(functionName) }),
    setError: (errorMessage) =>
      dispatch({
        ...new ChangeOutputAction(
          `Error: ${errorMessage}`,
          CommandType.CompileFunction,
          true
        ),
      }),
  };
};

export default connect(
  mapStateToProps,
  mapDispatchToProps
)(CompileFunctionPaneComponent);
