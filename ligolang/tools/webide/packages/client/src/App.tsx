import React, { useState } from 'react';
import { Provider } from 'react-redux';
import {
  BrowserRouter as Router,
  Switch,
  Route,
} from "react-router-dom";

import styled from 'styled-components';
import 'bootstrap/dist/css/bootstrap.min.css';
import SplitPane from 'react-split-pane';
import './index.css'

import { EditorComponent } from './components/editor/editor';
import Examples from './components/examples';
import { FloatButtonComponent } from './components/float-button';
import { HeaderComponent } from './components/header';
import { TabsPanelComponent } from './components/tabs-panel';
import { TooltipContainer } from './components/tooltip';
import OutputTab from './components/output/output-tab';
import configureStore from './configure-store';
import ViewSharedFile from './components/view-shared-file'
import ViewExampleFile from './components/view-example-file'

const store = configureStore();

interface TopPaneStyled {
  height: number;
}

const Wrapper = styled.div`
  display: flex;
  flex-direction: column;
  width: 100%;
  height: 100%;
`
const Container = styled.div`
  display: flex;
  flex-direction: row;
  padding: 0.5em 1em;
  min-width: 100%;
  min-height: 0;

  .SplitPane {
    display: flex;
    order: 2;
    width: inherit;
  }

  .Pane1 {
    display: flex;
    min-height: 0;
    overflow: auto;
    min-width: 100%;
  }

  .Pane2 {
    display: flex;
    min-height: 0;
    overflow: auto;
    min-width: 100%;
  }
`;

const FeedbackContainer = styled.div`
  display: flex;
  flex-direction: column;
  justify-content: space-between;
  align-items: flex-end;

  right: 0.5em;
  bottom: 1em;
  position: absolute;
`;

const EditorComponentWrapper = styled.div<TopPaneStyled>`
  height: ${props => props.height}px;
  overflow: auto;
  display: flex;
  min-width: 100%;
`

const App: React.FC = () => {

  const [topPaneHeight, setTopPaneHeight] = useState(700)

  return (
    <Provider store={store}>
      <Router>
        <Wrapper>
        <HeaderComponent />
        <Container>
        <div className="col-sm-12 col-md-2 order-md-1"><Examples /></div>
          <SplitPane onChange={size => setTopPaneHeight(size)} style={{ position: 'relative', display: 'flex' }} split="horizontal" defaultSize="75%" >
            <EditorComponentWrapper height={topPaneHeight} className="col-sm-12 col-md-7 order-md-2"><EditorComponent editorHeight={topPaneHeight} /></EditorComponentWrapper>
            <OutputTab />
          </SplitPane>
          <div className="col-sm-12 col-md-3 order-md-3"><TabsPanelComponent /></div>
        </Container>
        <FeedbackContainer>
          <FloatButtonComponent
            tooltip="Report an issue"
            text="!"
            href="https://gitlab.com/ligolang/ligo/-/issues/new?issue%5Bassignee_id%5D=&issue%5Bmilestone_id%5D="
          ></FloatButtonComponent>
          <FloatButtonComponent
            tooltip="Ask a question"
            text="?"
            href="https://t.me/LigoLang"
          ></FloatButtonComponent>
        </FeedbackContainer>
        <TooltipContainer />
      </Wrapper>
      <Switch>
      <Route path="/p/:id">
        <ViewSharedFile />
      </Route>
      <Route path="/">
        <ViewExampleFile />
      </Route>
      </Switch>
      </Router>
    </Provider>
  );
};

export default App;
