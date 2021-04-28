import {DocDragResizeRotate} from './Readme.mdx';

export default {
  title: 'Components/Drag Resize Rotate',
  parameters: {
    docs: {
      page: DocDragResizeRotate
    }
  },
  argTypes: {
    unit: {
      type: 'select',
      options: ['percentage', 'viewport', 'percentage']
    },
    drag: {
      type: 'select',
      options: ['x-axis', 'y-axis', 'all', 'none']
    },
    resize: {control: 'boolean'},
    rotation: {control: 'boolean'},
    text: {control: 'boolean'}
  }
};

export const DragResizeRotate = ({unit, drag, resize, rotation, text}) => {
  return `<div style="position: relative; width: 300px; height: 300px; background: #ccc">
  <deckgo-drr style="--width: 20%; --height: 10%; --top: 5%; --left: 10%; --rotate: 45deg"
              unit="${unit}" drag="${drag}" resize="${resize}" rotation="${rotation}" text="${text}">
    <div style="background: #FF0000;"></div>
  </deckgo-drr>
</div>`;
};

DragResizeRotate.args = {
  unit: 'percentage',
  drag: 'all',
  resize: true,
  rotation: true,
  text: false
};
