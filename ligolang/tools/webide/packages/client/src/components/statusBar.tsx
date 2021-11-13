import React from 'react';
import styled, { css } from 'styled-components';

const Container = styled.div<{ error: boolean }>`
  min-height: 40px;
  background-color: var(--blue_trans1);

  ${props =>
    props.error &&
    css`
      background-color: var(--orange_trans);
    `}
`;

export const Statusbar = (props: any) => {
  return <Container error={props.error}></Container>;
};
