import {Component, h, Event, EventEmitter, Prop} from '@stencil/core';

/**
 * @part edit-button: A CSS :part to access the button
 * @part edit-icon: A CSS :part to access the SVG icon rendered within the button
 */
@Component({
  tag: 'deckgo-highlight-code-edit',
  styleUrl: 'edit.scss',
  shadow: true
})
export class Edit {
  @Prop()
  label: string;

  @Event()
  editCode: EventEmitter<void>;

  render() {
    return (
      <button onClick={() => this.editCode.emit()} aria-label={this.label || 'Edit code'} part="edit-button">
        <svg xmlns="http://www.w3.org/2000/svg" height="24px" viewBox="0 0 24 24" width="24px" fill="currentColor" part="edit-icon">
          <path d="M0 0h24v24H0z" fill="none" />
          <path d="M3 17.25V21h3.75L17.81 9.94l-3.75-3.75L3 17.25zM20.71 7.04c.39-.39.39-1.02 0-1.41l-2.34-2.34c-.39-.39-1.02-.39-1.41 0l-1.83 1.83 3.75 3.75 1.83-1.83z" />
        </svg>
      </button>
    );
  }
}
