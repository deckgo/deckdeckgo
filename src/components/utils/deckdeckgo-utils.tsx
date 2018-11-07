export class DeckDeckGoUtils {

  static unifyEvent(e: any): any {
    return e.changedTouches ? e.changedTouches[0] : e;
  }

}
