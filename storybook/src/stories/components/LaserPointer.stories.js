import {Docs} from './LaserPointer.mdx';

export default {
  title: 'Components/Laser Pointer',
  parameters: {
    docs: {
      page: Docs
    }
  }
};

export const LaserPointer = () => {
  return `<deckgo-laser-pointer></deckgo-laser-pointer>`;
};

LaserPointer.args = {};
