import {Fragment, h, FunctionalComponent} from '@stencil/core';

interface AppActionsDocEditorProps {
  containerRef: HTMLElement | undefined;
}

export const AppActionsDocEditor: FunctionalComponent<AppActionsDocEditorProps> = ({containerRef}) => {
  return (
    <Fragment>
      <app-add-paragraph containerRef={containerRef}></app-add-paragraph>
      <app-transform-paragraph containerRef={containerRef}></app-transform-paragraph>
      <app-doc-indicator></app-doc-indicator>
    </Fragment>
  );
};
