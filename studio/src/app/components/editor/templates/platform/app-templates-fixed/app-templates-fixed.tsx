import {FunctionalComponent, Fragment, h} from '@stencil/core';

import {SlideTemplate} from '../../../../../models/data/slide';

import {InitTemplate} from '../../../../../utils/editor/create-slides.utils';

interface AppTemplatesFixedProps {
  selectTemplate: (template: InitTemplate) => Promise<void>;
}

export const AppTemplatesFixed: FunctionalComponent<AppTemplatesFixedProps> = ({selectTemplate}) => {
  const renderTitle = () => {
    return <app-templates-title custom-tappable onClick={() => selectTemplate({template: SlideTemplate.TITLE})}></app-templates-title>;
  };

  const renderContent = () => {
    const flexEndStyle = {'--slide-content-justify-content': 'flex-end'};

    return (
      <Fragment>
        <app-templates-content custom-tappable onClick={() => selectTemplate({template: SlideTemplate.CONTENT})}></app-templates-content>
        <app-templates-content
          custom-tappable
          onClick={() => selectTemplate({template: SlideTemplate.CONTENT, style: flexEndStyle})}
          style={flexEndStyle}></app-templates-content>
      </Fragment>
    );
  };

  const renderSplit = () => {
    return <app-templates-split custom-tappable onClick={() => selectTemplate({template: SlideTemplate.SPLIT})}></app-templates-split>;
  };

  const renderVertical = () => {
    return (
      <app-templates-split
        custom-tappable
        vertical={true}
        onClick={() => selectTemplate({template: SlideTemplate.SPLIT, attributes: {vertical: true}})}></app-templates-split>
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
