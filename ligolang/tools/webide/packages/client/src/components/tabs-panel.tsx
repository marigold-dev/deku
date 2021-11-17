import React, { useState } from 'react';
import styled from 'styled-components';

import { ConfigureTabComponent } from './configure/configure-tab';

const Container = styled.div`
  flex: 1;
  display: flex;
  flex-direction: column;
`;

const Header = styled.div`
  display: flex;
  min-height: 2.5em;
  margin-left: 15px;
  align-self: left;
  font-weight: 600;
`;

const Label = styled.span`
  user-select: none;
  flex: 1;
  display: flex;
  align-items: center;
  cursor: 
`;

const Tab = styled.div<{ selected?: boolean }>`
  flex: 1;
  display: flex;
  flex-direction: column;
`;

const Content = styled.div`
  width: 100%;
  height: 100%;
  overflow: hidden;
`;

export const TabsPanelComponent = () => {
  const TABS = [
    { index: 0, label: 'Configure', id: 'configure-tab' }
  ];

  const [selectedTab, selectTab] = useState(TABS[0]);

  return (
    <Container>
      <Header>
        {TABS.map(tab => (
          <Tab
            key={tab.id}
            id={tab.id}
            selected={true}
          >
            <Label onClick={() => selectTab(tab)}>{tab.label}</Label>
          </Tab>
        ))}
      </Header>
      <Content>
        <ConfigureTabComponent
          selected={true}
          onRun={() => {
            selectTab(TABS[1]);
          }}
        ></ConfigureTabComponent>
        {/* <OutputTab
          selected={selectedTab.index === 1}
          onCancel={() => {
            selectTab(TABS[0]);
          }}
        ></OutputTab> */}
      </Content>
    </Container>
  );
};
