import React from 'react';
import styled from 'styled-components';

export const Group = styled.div`
  display: flex;
  flex-direction: column;
  margin: 0.7em 0 0.7em 0;
`;

export const HGroup = styled.div`
  display: flex;
  align-items: center;
  margin: 0.4em 0 0.4em 0;
`;

export const Label = styled.label`
  font-size: 1em;
  color: var(--label_foreground);
  user-select: none;
  margin: 0.3em 0 0.3em 0;
`;

export const Hint = styled.span`
  font-style: italic;
  font-size: 0.8em;
`;

export const AccessFunctionLabel = (props: any) => {
  return (
    <Label {...props}>
      Access function
      <br />
      <Hint>The function name from where your contract will start</Hint>
    </Label>
  );
};

export const Input = styled.input`
  /* margin: 0.3em 0 0.7em 0; */
  background-color: var(--input_background);
  border-style: none;
  border-bottom: 5px solid #e1f1ff;
  padding: 0.5em;
  font-size: 1em;
  font-family: Menlo, Monaco, 'Courier New', monospace;
  outline: none;

  &:focus {
    background-color: #e1f1ff;
  }
`;

export const Textarea = styled.textarea`
  resize: vertical;
  /* margin: 0.3em 0 0.7em 0; */
  background-color: var(--input_background);
  border-style: none;
  border-bottom: 5px solid #e1f1ff;
  padding: 0.5em;
  font-size: 1em;
  font-family: Menlo, Monaco, 'Courier New', monospace;
  outline: none;

  &:focus {
    background-color: #e1f1ff;
  }
`;
