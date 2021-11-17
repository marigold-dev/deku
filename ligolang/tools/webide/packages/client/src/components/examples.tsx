import React, {FC, useEffect, useState} from 'react';
import { useDispatch, connect } from 'react-redux';
import styled from 'styled-components';
import 'bootstrap/dist/css/bootstrap.min.css'

import { ChangeDirtyAction } from '../redux/editor';
import { ChangeSelectedAction, ExampleItem } from '../redux/examples';
import { getExample } from '../services/api';
import { ExampleAction, ExampleListAction } from '../redux/actions/examples'

const Container = styled.div`
  flex: 0.5;  
  display: flex;
  flex-direction: column;
  min-width: 0;
  order: 1;
`;

const Header = styled.div`
  min-height: 2.5em;
  display: flex;
  align-items: center;
  font-weight: 600;
`;

const MenuContainer = styled.div`
  display: flex;
  flex-direction: column;
  overflow-y: auto;
  height: var(--content_height);
  font-size: 0.8em;
`;

const MenuItem = styled.span`
 
  padding: 0.6em;
  cursor: pointer;
  background-color: transparent;
  :hover {
    background-color: var(--blue_trans2);
  }
`;

interface stateTypes {
  isEditorDirty?: boolean;
}

interface dispatchTypes {
  defaultExampleList: () => any;
  getExample: (id) => any
}

const Examples:FC<stateTypes&dispatchTypes> = (props) => {
  
  const { isEditorDirty } = props
  const [exampleList, setExampleList] = useState<ExampleItem[]>([]);
  const dispatch = useDispatch();

  useEffect(() => {
    props.defaultExampleList().then((list) => {
      setExampleList(list)
    })
  },[props]);

  return (
    <Container>
      <Header>Contract Examples</Header>
      <MenuContainer>
        {exampleList && exampleList.map(example => {
          return (
            <MenuItem
              id={example.id}
              key={example.id}
              onClick={async () => {
                const response = await getExample(example.id);
                if (
                  !isEditorDirty ||
                  window.confirm(
                    'Are you sure you want to navigate away? Data you have entered will be lost.\n\nPress OK to continue or Cancel to stay on the current page.\n\n'
                  )
                 ) {
                  dispatch({ ...new ChangeSelectedAction(response) });
                  dispatch({ ...new ChangeDirtyAction(false) });
                }
              }}
            >
              {example.name}
            </MenuItem>
          );
        })}
      </MenuContainer>
    </Container>
  );
};

const mapStateToProps = state => {
  const { editor } = state
  return { 
    isEditorDirty : editor.dirty
   }
}

const mapDispatchToProps = dispatch => {
  return({
    defaultExampleList: ()  => dispatch(ExampleListAction()),
    getExample: (id)  => dispatch(ExampleAction(id))
  })
}

export default connect(mapStateToProps, mapDispatchToProps)(Examples)