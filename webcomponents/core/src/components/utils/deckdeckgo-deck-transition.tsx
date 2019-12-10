import {h, FunctionalComponent} from '@stencil/core';

export const HideSlides: FunctionalComponent = () => {
  return (
    <style>{`
      ::slotted(.deckgo-slide-container) {
        opacity: 0;
        visibility: hidden;
        transition: all var(--transition-fade-duration, 500ms);
      };
    `}</style>
  );
};

export const RevealSlide: FunctionalComponent<{ index: number }> = ({index}) => {
  return (
    <style>{`
      ::slotted(.deckgo-slide-container:nth-child(${index+1})) {
        visibility: initial;
        opacity: 1;
      };
    `}</style>
  );
};
