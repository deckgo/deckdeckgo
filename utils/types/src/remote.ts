export enum DeckdeckgoDrawAction {
  PENCIL,
  CIRCLE
}

export enum DeckdeckgoSlideAction {
  PLAY = 'play',
  PAUSE = 'pause'
}

export interface DeckdeckgoAttributeDefinition {
  name: string;
  value?: any;
}

export interface DeckdeckgoSlideDefinition {
  template: string | undefined;
  content?: string;
  attributes?: DeckdeckgoAttributeDefinition[] | null;
}

export interface DeckdeckgoDeckDefinition {
  slides: DeckdeckgoSlideDefinition[];
  attributes?: DeckdeckgoAttributeDefinition[];
  background?: string;

  reveal: boolean;
  revealOnMobile: boolean;
}

export enum DeckdeckgoEventType {
  SLIDES_REQUEST = 'slides_request',
  SLIDES_ANSWER = 'slides_answer',
  DECK_UPDATE = 'deck_update',
  SLIDE_UPDATE = 'slide_update',
  NEXT_SLIDE = 'next_slide',
  PREV_SLIDE = 'prev_slide',
  SLIDE_TO = 'slide_to',
  CLEAR_SLIDE = 'clear_slide',
  START_DRAWING = 'start_drawing',
  DRAW = 'draw',
  END_DRAWING = 'end_drawing',
  SLIDE_ACTION = 'slide_action',
  DELETE_SLIDE = 'delete_slide',
  DECK_REVEAL_UPDATE = 'deck_reveal_update'
}

export enum DeckdeckgoEventEmitter {
  DECK = 'deck',
  APP = 'app'
}

export interface DeckdeckgoEvent {
  type: DeckdeckgoEventType;
  emitter: DeckdeckgoEventEmitter;
}

export interface DeckdeckgoEventDraw extends DeckdeckgoEvent {
  action: DeckdeckgoDrawAction;

  clientX: number;
  clientY: number;

  windowWidth: number;
  windowHeight: number;

  color?: string;
}

export interface DeckdeckgoEventNextPrevSlide extends DeckdeckgoEvent {
  slideAnimation: boolean;
}

export interface DeckdeckgoEventDeckReveal extends DeckdeckgoEvent {
  reveal: boolean;
}

export interface DeckdeckgoEventSlideTo extends DeckdeckgoEvent {
  index: number;
  speed?: number;
}

export interface DeckdeckgoEventDeck extends DeckdeckgoEvent {
  length: number;
  mobile: boolean;
  deck: DeckdeckgoDeckDefinition;
}

export interface DeckdeckgoEventSlide extends DeckdeckgoEvent {
  index: number;
  slide: DeckdeckgoSlideDefinition;
}

export interface DeckdeckgoEventSlideAction extends DeckdeckgoEvent {
  action: DeckdeckgoSlideAction;
}
