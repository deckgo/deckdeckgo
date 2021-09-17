import {UserData} from '@deckdeckgo/editor';

// For Firebase we enhance the user with our API information without saving these info into the Firestore database
// Long story short: we use a SQL database at AWS to make username unique

export interface ApiUserData extends UserData {
  apiUserId: string;
}
