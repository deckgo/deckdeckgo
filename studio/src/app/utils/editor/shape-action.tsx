import {ImageAction} from './image-action';

export interface ShapeActionSVG {
  src: string;
  label: string;
  ratio: number;
}

export interface ShapeAction {
  svg?: ShapeActionSVG;
  img?: ImageAction;
}
