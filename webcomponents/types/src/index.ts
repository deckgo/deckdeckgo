export enum DeckdeckgoDrawAction {
    PENCIL,
    CIRCLE
}

export enum DeckdeckgoSlideAction {
    PLAY = 'play',
    PAUSE = 'pause'
}

export interface DeckdeckgoSlideDefinition {
    name: string;
    title?: string;
    notes?: string;
}

export enum DeckdeckgoEventType {
    SLIDES_REQUEST = 'slides_request',
    SLIDES_ANSWER = 'slides_answer',
    SLIDES_UPDATE = 'slides_update',
    NEXT_SLIDE = 'next_slide',
    PREV_SLIDE = 'prev_slide',
    SLIDE_TO = 'slide_to',
    CLEAR_SLIDE = 'clear_slide',
    START_DRAWING = 'start_drawing',
    DRAW = 'draw',
    END_DRAWING = 'end_drawing',
    SLIDE_ACTION = 'slide_action',
    DELETE_SLIDE = 'delete_slide'
}

export enum DeckdeckgoEventEmitter {
    DECK = 'deck',
    APP = 'app'
}

export interface DeckdeckgoEvent {
    type: DeckdeckgoEventType;
    emitter: DeckdeckgoEventEmitter
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

export interface DeckdeckgoEventSlideTo extends DeckdeckgoEvent {
    index: number;
    speed?: number
}

export interface DeckdeckgoEventSlides extends DeckdeckgoEvent {
    length: number;
    slides: DeckdeckgoSlideDefinition[];
}

export interface DeckdeckgoEventSlideAction extends DeckdeckgoEvent {
    action: DeckdeckgoSlideAction;
}
