import {BehaviorSubject, Observable} from 'rxjs';

export class MenuService {

  private static instance: MenuService;

  private enableSubject: BehaviorSubject<boolean> = new BehaviorSubject(true);

  private constructor() {
    // Private constructor, singleton
  }

  static getInstance() {
    if (!MenuService.instance) {
      MenuService.instance = new MenuService();
    }
    return MenuService.instance;
  }

  watch(): Observable<boolean> {
    return this.enableSubject.asObservable();
  }

  isEnable(): boolean {
    return this.enableSubject.getValue();
  }

  disable() {
    this.enableSubject.next(false);
  }

  enable() {
    this.enableSubject.next(true);
  }

}
