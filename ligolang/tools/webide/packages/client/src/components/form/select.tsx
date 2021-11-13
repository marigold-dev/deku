import { faCaretDown } from '@fortawesome/free-solid-svg-icons';
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome';
import React, { FunctionComponentElement, useState } from 'react';
import OutsideClickHandler from 'react-outside-click-handler';
import styled, { css } from 'styled-components';

const Container = styled.div`
  display: flex;
  position: relative;
  min-width: 8em;
`;

const Header = styled.div`
  cursor: pointer;
  user-select: none;

  flex: 1;
  display: flex;
  justify-content: space-between;
  align-items: center;
  min-height: 2em;
  padding: 0 0.5em;

  border: 1px solid var(--blue_trans1);
`;

const ArrowIcon = ({ rotate, ...props }: { rotate: boolean }) => (
  <FontAwesomeIcon {...props} icon={faCaretDown} size="lg"></FontAwesomeIcon>
);

const Arrow = styled(ArrowIcon)`
  z-index: 1;
  pointer-events: none;
  color: var(--blue_trans1);
  transition: transform 0.15s ease-in;

  ${(props: { rotate: boolean }) =>
    props.rotate &&
    css`
      transform: rotate(180deg);
    `};
`;

const List = styled.ul`
  z-index: 1;
  position: absolute;
  list-style-type: none;
  background-color: white;
  width: 100%;
  margin: 0;
  padding: 0;
  box-shadow: 1px 3px 10px 0px rgba(153, 153, 153, 0.4);
  border-radius: 3px;

  visibility: hidden;
  opacity: 0;
  transition: opacity 0.15s ease-in;

  ${(props: { visible: boolean }) =>
    props.visible &&
    css`
      visibility: visible;
      opacity: 1;
    `}
`;

const OptionContainer = styled.li`
  cursor: pointer;
  user-select: none;

  display: flex;
  align-items: center;
  justify-content: space-between;
  height: 2em;
  padding: 0 0.5em;

  &:first-child {
    border-radius: 3px 3px 0 0;
  }

  &:last-child {
    border-radius: 0 0 3px 3px;
  }

  &:hover {
    background-color: var(--blue_trans2);
    font-weight: 600;
  }
`;

interface OptionProps {
  value: string;
  children: string;
}

type OptionElement = FunctionComponentElement<OptionProps>;

export const Option = (props: OptionProps) => {
  // This is an empty component. It's used as a way to get option information into its parent. It is not inserted into the DOM.
  return <></>;
};

export const Select = (props: {
  id: string;
  value: any;
  children: OptionElement[] | OptionElement;
  onChange?: (value: any) => void;
  className?: string;
}) => {
  const [isOpen, setOpen] = useState(false);

  const options = Array.isArray(props.children)
    ? props.children
    : [props.children];

  const labelLookup = new Map(
    options.map(
      child => [child.props.value, child.props.children] as [string, string]
    )
  );

  const moveOptionToTop = (value: string) => {
    return options.reduce((list, entry) => {
      if (entry.props.value === value) {
        list.unshift(entry);
      } else {
        list.push(entry);
      }
      return list;
    }, [] as OptionElement[]);
  };

  const selectOption = (option: OptionElement) => {
    if (props.value !== option.props.value && props.onChange) {
      props.onChange(option.props.value);
    }
    setOpen(false);
  };

  return (
    <Container className={props.className}>
      <OutsideClickHandler onOutsideClick={() => setOpen(false)}>
        <List visible={isOpen}>
          {moveOptionToTop(props.value).map((option: OptionElement) => (
            <OptionContainer
              id={option.props.value}
              key={option.props.value}
              onClick={() => selectOption(option)}
            >
              <span>{option.props.children}</span>
            </OptionContainer>
          ))}
        </List>
      </OutsideClickHandler>
      <Header id={props.id} onClick={() => setOpen(true)}>
        <span>{labelLookup.get(props.value)}</span>
        <Arrow rotate={isOpen}></Arrow>
      </Header>
    </Container>
  );
};
