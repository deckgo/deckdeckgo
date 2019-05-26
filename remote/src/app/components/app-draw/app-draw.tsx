import {Component, Element, Method, Prop, State, Watch, EventEmitter, Event} from '@stencil/core';

// Types
import {DeckdeckgoDrawAction, DeckdeckgoEventType, DeckdeckgoEventEmitter} from '@deckdeckgo/types';

// Services
import {CommunicationService} from '../../services/communication/communication.service';

@Component({
    tag: 'app-draw',
    styleUrl: 'app-draw.scss',
    shadow: true
})
export class AppDraw {
    @Element() el: HTMLElement;

    @Prop() width: number;
    @Prop() height: number;
    @Prop() heightOffset: number = 0;
    @Prop() slides: number;

    @State() private canvasWidth: number;

    private canvas: HTMLCanvasElement;
    private ctx: CanvasRenderingContext2D;

    private startX: number;
    private startY: number;

    private leftOffset: number = 0;

    private drawEvents: boolean = false;
    private drawAction: boolean = false;

    @State() action: DeckdeckgoDrawAction = DeckdeckgoDrawAction.PENCIL;

    @Event() drawing: EventEmitter<boolean>;

    @State() private color: string = 'red';

    constructor(private communicationService: CommunicationService) {
        this.communicationService = CommunicationService.getInstance();
    }

    async componentDidLoad() {
        this.initCanvasWidth();

        await this.initContext();
    }

    @Watch('width')
    @Watch('slides')
    private initCanvasWidth() {
        if (this.width && this.slides) {
            this.canvasWidth = this.width * this.slides;
        }
    }

    private initContext(): Promise<void> {
        return new Promise<void>((resolve) => {
            const canvas: HTMLCanvasElement = this.el.shadowRoot.querySelector('canvas');

            if (!canvas) {
                resolve();
                return;
            }

            this.ctx = canvas.getContext("2d");

            resolve();
        });
    }

    @Method()
    moveDraw(leftOffset: number, transitionDuration: string): Promise<void> {
        return new Promise<void>((resolve) => {
            const canvas: HTMLCanvasElement = this.el.shadowRoot.querySelector('canvas');

            if (!canvas) {
                resolve();
                return;
            }

            this.leftOffset = leftOffset;

            canvas.style.setProperty('--left-offset', '' + this.leftOffset + 'px');
            canvas.style.setProperty('--left-offset-transition', transitionDuration);

            resolve();
        });
    }

    private start(): Promise<void> {
        return new Promise<void>((resolve) => {
            this.canvas = this.el.shadowRoot.querySelector('canvas');

            if (!this.canvas) {
                resolve();
                return;
            }

            this.canvas.style.zIndex = '1';

            this.canvas.addEventListener('mousedown', this.startEvent, {passive: true});
            this.canvas.addEventListener('touchstart', this.startEvent, {passive: true});
            this.canvas.addEventListener('mouseup', this.endEvent, {passive: true});
            this.canvas.addEventListener('touchend', this.endEvent, {passive: true});
            this.canvas.addEventListener('mousemove', this.drawEvent, {passive: true});
            this.canvas.addEventListener('touchmove', this.drawEvent, {passive: true});

            resolve();
        });
    }

    private end(): Promise<void> {
        return new Promise<void>((resolve) => {
            this.canvas = this.el.shadowRoot.querySelector('canvas');

            if (!this.canvas) {
                resolve();
                return;
            }

            this.canvas.style.zIndex = '0';

            this.canvas.removeEventListener('mousedown', this.startEvent, true);
            this.canvas.removeEventListener('touchstart', this.startEvent, true);
            this.canvas.removeEventListener('mouseup', this.endEvent, true);
            this.canvas.removeEventListener('touchend', this.endEvent, true);
            this.canvas.removeEventListener('mousemove', this.drawEvent, true);
            this.canvas.removeEventListener('touchmove', this.drawEvent, true);

            resolve();
        });
    }

    private startEvent = (e: MouseEvent) => {
        this.emit(DeckdeckgoEventType.START_DRAWING, e);

        this.startX = this.unifyEvent(e).clientX - this.leftOffset;
        this.startY = this.unifyEvent(e).clientY - this.heightOffset;

        this.drawEvents = true;
    };

    private endEvent = (e: MouseEvent) => {
        this.emit(DeckdeckgoEventType.END_DRAWING, e);

        this.drawEvents = false;
    };

    private drawEvent = (e: MouseEvent) => {
        if (!this.drawEvents) {
            return;
        }

        this.emit(DeckdeckgoEventType.DRAW, e);

        const toX: number = this.unifyEvent(e).clientX - this.leftOffset;
        const toY: number = this.unifyEvent(e).clientY - this.heightOffset;

        this.draw(toX, toY);
    };

    private unifyEvent(e: any): any {
        return e.changedTouches ? e.changedTouches[0] : e;
    }

    private draw(toX: number, toY: number) {
        this.ctx.beginPath();

        if (this.action === DeckdeckgoDrawAction.CIRCLE) {
            this.drawCircle(toX, toY);
        } else {
            this.drawPencil(toX, toY);
        }

        this.ctx.strokeStyle = this.color;
        this.ctx.lineWidth = 3;

        this.ctx.stroke();
        this.ctx.closePath();
    }

    private drawPencil(toX: number, toY: number) {
        this.ctx.moveTo(this.startX, this.startY);
        this.ctx.lineTo(toX, toY);

        this.startX = toX;
        this.startY = toY;
    }

    private drawCircle(toX: number, toY: number) {
        this.ctx.clearRect(-1 * this.leftOffset, 0, this.width, this.height);
        this.ctx.moveTo(this.startX, this.startY + (toY - this.startY) / 2);
        this.ctx.bezierCurveTo(this.startX, this.startY, toX, this.startY, toX, this.startY + (toY - this.startY) / 2);
        this.ctx.bezierCurveTo(toX, toY, this.startX, toY, this.startX, this.startY + (toY - this.startY) / 2);
    }

    private switchTool(e: UIEvent, action: DeckdeckgoDrawAction) {
        e.stopPropagation();

        this.action = action;
    }

    private switchColor(e: UIEvent) {
        e.stopPropagation();

        this.color = this.color === 'red' ? 'black' : 'red';
    }

    private async startStopDrawing() {
        if (this.drawAction) {
            await this.end();
        } else {
            await this.start();
        }

        this.drawAction = !this.drawAction;

        this.drawing.emit(this.drawAction);
    }

    private emit(type: DeckdeckgoEventType, e: any) {
        this.communicationService.emit({
            type: type,
            emitter: DeckdeckgoEventEmitter.APP,
            action: this.action,
            clientX: this.unifyEvent(e).clientX,
            clientY: this.unifyEvent(e).clientY - this.heightOffset,
            windowWidth: this.width,
            windowHeight: this.height,
            color: this.color
        });
    }

    private clear(e: UIEvent): Promise<void> {
        return new Promise<void>((resolve) => {
            e.stopPropagation();

            this.communicationService.emit({type: DeckdeckgoEventType.CLEAR_SLIDE, emitter: DeckdeckgoEventEmitter.APP});

            this.ctx.beginPath();
            this.ctx.clearRect(-1 * this.leftOffset, 0, this.width, this.height);
            this.ctx.stroke();
            this.ctx.closePath();

            resolve();
        });
    }

    render() {

        const styleColorPicker = {
          color: this.color === 'red' ? 'black' : 'red'
        };

        return ([
            <canvas width={this.canvasWidth} height={this.height}></canvas>,
            <ion-fab vertical="bottom" horizontal="start" slot="fixed">
                <ion-fab-button onClick={() => this.startStopDrawing()}>
                    <ion-icon name="brush"></ion-icon>
                </ion-fab-button>
                <ion-fab-list side="end">
                    {this.renderPencilRubber()}
                </ion-fab-list>
                <ion-fab-list side="top">
                    <ion-fab-button color="medium" style={styleColorPicker} onClick={(e: UIEvent) => this.switchColor(e)}>
                        <ion-icon name="color-palette"></ion-icon>
                    </ion-fab-button>
                    <ion-fab-button color="medium" onClick={(e: UIEvent) => this.clear(e)}>
                        <ion-icon name="trash"></ion-icon>
                    </ion-fab-button>
                </ion-fab-list>
            </ion-fab>
        ]);
    }

    private renderPencilRubber() {
        if (this.action !== DeckdeckgoDrawAction.PENCIL) {
            return (
                <ion-fab-button color="medium" onClick={(e: UIEvent) => this.switchTool(e, DeckdeckgoDrawAction.PENCIL)}>
                    <ion-icon name="create"></ion-icon>
                </ion-fab-button>
            );
        } else {
            return (
                <ion-fab-button color="medium" onClick={(e: UIEvent) => this.switchTool(e, DeckdeckgoDrawAction.CIRCLE)}>
                    <ion-icon name="radio-button-off"></ion-icon>
                </ion-fab-button>
            );
        }
    }
}
