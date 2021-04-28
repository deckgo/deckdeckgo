import {DocLaserPointer} from './Readme.mdx';

export default {
  title: 'Components/Laser Pointer',
  parameters: {
    docs: {
      page: DocLaserPointer
    }
  }
};

export const LaserPointer = () => {
  return `<deckgo-laser-pointer></deckgo-laser-pointer>`;
};

LaserPointer.args = {};
