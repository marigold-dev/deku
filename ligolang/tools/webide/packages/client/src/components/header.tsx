import React from 'react';
import styled, { css } from 'styled-components';
require('typeface-inter')

const Container = styled.div`
  display: flex;
  justify-content: space-between;
  align-items: center;

  padding: 0.3em 1em;
  margin-bottom: 1em;
  font-family: 'DM Sans', 'Open Sans', sans-serif;

  box-shadow: 0px 2px 2px 0px rgba(0, 0, 0, 0.3);
  height: 60px;
  align-items: center;
`;

const Group = styled.div`
  display: flex;
  align-items: center;
`;

const Logo = styled.img`
  height: 32px;
`;

const Link = styled.a`
  text-decoration: none;
  color: black;
  padding: 0.5em 1em;
  margin: 0.5em;
  font-family: 'Inter var';
  font-weight: 500;

  &:hover {
    color: #0e74ff;
    text-decoration: none;
  }

  ${(props: { cheatSheetStyle?: boolean }) =>
    props.cheatSheetStyle &&
    css`
      background-color: #efefef;
      margin-left: 3em;
      border-radius: 25px;
      &:hover {
        color: black;
      }
    `}
`;

export const HeaderComponent = () => {
  return (
    <Container className="navbar navbar-default navbar-fixed-top">
      <Group className="navbar-header">
        <a href="https://ligolang.org" style={{margin: "1em"}}>
          <Logo src="/logo.svg" />
        </a>
        
        <Link href="https://ligolang.org/docs/intro/installation">Install</Link>
        <Link href="https://ligolang.org/docs/intro/introduction">Docs</Link>
        <Link href="https://ligolang.org/docs/tutorials/get-started/tezos-taco-shop-smart-contract">
          Tutorials
        </Link>
        <Link href="https://forum.tezosagora.org/tag/ligo" target="_blank">Blog</Link>
        <Link href="https://ligolang.org/contact">
          Ask Questions
        </Link>
      
      </Group>
      <Link cheatSheetStyle href="https://ligolang.org/docs/api/cheat-sheet" target="_blank">
          Cheat Sheet
        </Link>
    </Container>
  );
};
