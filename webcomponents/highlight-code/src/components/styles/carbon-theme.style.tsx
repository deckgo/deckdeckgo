import {FunctionalComponent, h} from '@stencil/core';

export const CarbonThemeStyle: FunctionalComponent<{style: string}> = ({style}) => {
  return (
    <style>{`
      :host ${style};
    `}</style>
  );
};
