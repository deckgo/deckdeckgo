import {Component, Element, Event, EventEmitter, Fragment, h} from '@stencil/core';

import {SlideTemplate} from '../../../../../models/data/slide';

import {InitTemplate} from '../../../../../utils/editor/create-slides.utils';

@Component({
  tag: 'app-templates-fixed',
})
export class AppTemplatesFixed {
  @Element() el: HTMLElement;

  @Event()
  selectedTemplate: EventEmitter<InitTemplate>;

  render() {
    return (
      <Fragment>
        {this.renderTitle()}
        {this.renderContent()}
        {this.renderSplit()}
        {this.renderVertical()}
      </Fragment>
    );
  }

  private renderTitle() {
    return <app-templates-title custom-tappable onClick={() => this.selectedTemplate.emit({template: SlideTemplate.TITLE})}></app-templates-title>;
  }

  private renderContent() {
    const flexEndStyle = {'--slide-content-justify-content': 'flex-end'};

    return (
      <Fragment>
        <app-templates-content custom-tappable onClick={() => this.selectedTemplate.emit({template: SlideTemplate.CONTENT})}></app-templates-content>
        <app-templates-content
          custom-tappable
          onClick={() => this.selectedTemplate.emit({template: SlideTemplate.CONTENT, style: flexEndStyle})}
          style={flexEndStyle}></app-templates-content>
      </Fragment>
    );
  }

  private renderSplit() {
    return <app-templates-split custom-tappable onClick={() => this.selectedTemplate.emit({template: SlideTemplate.SPLIT})}></app-templates-split>;
  }

  private renderVertical() {
    return (
      <app-templates-split
        custom-tappable
        vertical={true}
        onClick={() => this.selectedTemplate.emit({template: SlideTemplate.SPLIT, attributes: {vertical: true}})}></app-templates-split>
    );
  }
}
