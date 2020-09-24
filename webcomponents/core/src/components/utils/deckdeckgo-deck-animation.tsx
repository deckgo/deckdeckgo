import {h, FunctionalComponent} from '@stencil/core';

export const HideSlides: FunctionalComponent = () => {
  // Note: a start (hidden) opacity of 0 would has for effect to produce some "white flash" on transition.
  // Setting a value like 0.4 reduce the flash effect and makes the transition smoother for my eyes.

  // Note: if --slide-transition is not defined we have to set a value otherwise the property isn't a valid property.
  // Therefore we add a transition on visibility, it has no impact.

  return (
    <style>{`
      ::slotted(.deckgo-slide-container) {
        opacity: var(--transition-fade-hidden-opacity, 0.4);
        visibility: hidden;
        transition: opacity var(--transition-fade-duration, 500ms) cubic-bezier(0.23, 1, 0.320, 1), var(--slide-transition, visibility 500ms);
      };
    `}</style>
  );
};

export const RevealSlide: FunctionalComponent<{index: number}> = ({index}) => {
  return (
    <style>{`
      ::slotted(.deckgo-slide-container:nth-child(${index + 1})) {
        visibility: initial;
        opacity: 1;
      };
    `}</style>
  );
};

export const AnimationSlide: FunctionalComponent = () => {
  return (
    <style>{`
      ::slotted(.deckgo-slide-container) {
        transition: var(--slide-transition);
      }
    `}</style>
  );
};
