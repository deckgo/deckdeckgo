import {FunctionalComponent, Fragment, h} from '@stencil/core';

import {SlideTemplate} from '@deckdeckgo/editor';

import {InitTemplate} from '../../../../../../../utils/editor/create-slides.utils';

interface AppTemplatesFixedProps {
  selected?: 'title' | 'content-default' | 'content-bottom' | 'split-horizontal' | 'split-vertical';
  selectTemplate: (template: InitTemplate) => Promise<void>;
}

export const AppTemplatesFixed: FunctionalComponent<AppTemplatesFixedProps> = ({selectTemplate, selected}) => {
  const renderTitle = () => {
    return (
      <app-templates-title
        custom-tappable
        selected={selected === 'title'}
        onClick={() => selectTemplate({template: SlideTemplate.TITLE})}
      ></app-templates-title>
    );
  };

  const renderContent = () => {
    const flexEndStyle = {'--slide-content-justify-content': 'flex-end'};

    return (
      <Fragment>
        <app-templates-content
          custom-tappable
          selected={selected === 'content-default'}
          onClick={() => selectTemplate({template: SlideTemplate.CONTENT})}
        ></app-templates-content>
        <app-templates-content
          custom-tappable
          selected={selected === 'content-bottom'}
          onClick={() => selectTemplate({template: SlideTemplate.CONTENT, style: flexEndStyle})}
          style={flexEndStyle}
        ></app-templates-content>
      </Fragment>
    );
  };

  const renderSplit = () => {
    return (
      <app-templates-split
        custom-tappable
        selected={selected === 'split-horizontal'}
        onClick={() => selectTemplate({template: SlideTemplate.SPLIT})}
      ></app-templates-split>
    );
  };

  const renderVertical = () => {
    return (
      <app-templates-split
        custom-tappable
        selected={selected === 'split-vertical'}
        vertical={true}
        onClick={() => selectTemplate({template: SlideTemplate.SPLIT, attributes: {vertical: true}})}
      ></app-templates-split>
    );
  };

  return (
    <Fragment>
      {renderTitle()}
      {renderContent()}
      {renderSplit()}
      {renderVertical()}
    </Fragment>
  );
};
