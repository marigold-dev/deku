import React, { createElement, useCallback, useEffect, useRef, useState } from 'react';
import { render } from 'react-dom';
import styled from 'styled-components';

const Container = styled.div`
  position: fixed;
  z-index: 1000;
  top: 0;
  left: 0;
  height: 100%;
  width: 100%;
  pointer-events: none;
`;

export const StyledTooltip = styled.div<{
  visible: boolean;
  x: string;
  y: string;
}>`
  position: fixed;
  pointer-events: none;
  z-index: 1001;
  font-size: var(--font_sub_size);
  color: var(--tooltip_foreground);
  background-color: var(--tooltip_background);
  border-radius: 6px;
  padding: 5px 10px;
  opacity: 0;
  transition: opacity 0.2s ease 0.2s;
  transform-origin: center;

  ${({ x, y }) => `transform: translate(calc(${x}), calc(${y}));`}
  ${({ visible }) => visible && `opacity: 1;`}
`;

const TOOLTIP_CONTAINER_ID = 'tooltip-container';
type Position = 'top' | 'bottom' | 'left' | 'right';

export const TooltipContainer = () => {
  return <Container id={TOOLTIP_CONTAINER_ID}></Container>;
};

function calcX(triggerRect: ClientRect, position?: Position) {
  if ('left' === position) {
    return `${triggerRect.left - 10}px - 100%`;
  } else if ('right' === position) {
    return `${triggerRect.right + 10}px`;
  }

  return `${triggerRect.left + triggerRect.width / 2}px - 50%`;
}

function calcY(triggerRect: ClientRect, position?: string) {
  if ('top' === position) {
    return `${triggerRect.top - 10}px - 100%`;
  } else if (!position || 'bottom' === position) {
    return `${triggerRect.bottom + 10}px`;
  }

  return `${triggerRect.top + triggerRect.height / 2}px - 50%`;
}

export const Tooltip = (props: { position?: Position; children: any }) => {
  const ref = useRef<HTMLDivElement>(null);
  const [isTooltipVisible, setTooltipVisible] = useState(false);

  const renderTooltip = useCallback(
    (visible: boolean, triggerRect: ClientRect) => {
      const tooltip = createElement(
        StyledTooltip,
        {
          visible,
          x: calcX(triggerRect, props.position),
          y: calcY(triggerRect, props.position)
        },
        props.children
      );

      render(tooltip, document.getElementById(TOOLTIP_CONTAINER_ID));
    },
    [props.position, props.children]
  );

  useEffect(() => {
    if (ref.current) {
      const trigger = (ref.current as HTMLElement).parentElement;

      if (trigger) {
        if (isTooltipVisible) {
          renderTooltip(true, trigger.getBoundingClientRect());
        }

        trigger.onmouseenter = _ => {
          renderTooltip(true, trigger.getBoundingClientRect());
          setTooltipVisible(true);
        };

        trigger.onmouseleave = _ => {
          renderTooltip(false, trigger.getBoundingClientRect());
          setTooltipVisible(false);
        };
      }
    }
  }, [isTooltipVisible, renderTooltip]);

  return <div ref={ref}></div>;
};
