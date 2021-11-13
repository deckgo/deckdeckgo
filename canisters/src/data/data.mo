import HashMap "mo:base/HashMap";
import Text "mo:base/Text";
import Iter "mo:base/Iter";

import Result "mo:base/Result";
import Error "mo:base/Error";

import Types "../types/types";
import DataTypes "./data.types";

import Utils "../utils/utils";

import WalletUtils "../utils/wallet.utils";

import DataStore "./data.store";

actor class DataBucket(owner: Types.UserId) = this {

  type UserId = Types.UserId;

  type Data = DataTypes.Data;

  private stable let user: Types.UserId = owner;

  private let walletUtils: WalletUtils.WalletUtils = WalletUtils.WalletUtils();

  private let store: DataStore.DataStore<Data> = DataStore.DataStore<Data>();

  // Preserve the application state on upgrades
  private stable var entries : [(Text, Data)] = [];

   /**
    * Data
    */

  public shared query({ caller }) func get(key: Text) : async (Data) {
    if (Utils.isPrincipalNotEqual(caller, user)) {
        throw Error.reject("User does not have the permission to get the data.");
    };

    let result: Result.Result<Data, Text> = store.get(key);

    switch (result) {
        case (#err error) {
            throw Error.reject(error);
        };
        case (#ok value) {
            return value;
        };
    };
  };

  public shared query({ caller }) func list(filter: ?Text) : async [Data] {
      if (Utils.isPrincipalNotEqual(caller, user)) {
        throw Error.reject("User does not have the permission to list the data.");
    };

    let results: [Data] = store.entries(filter);
    return results;
  };

  public shared({ caller }) func set(key: Text, data: Data) : async () {
    if (Utils.isPrincipalNotEqual(caller, user)) {
        throw Error.reject("User does not have the permission to set data.");
    };

    store.put(key, data);
  };

  public shared({ caller }) func del(key: Text) : async () {
    if (Utils.isPrincipalNotEqual(caller, user)) {
        throw Error.reject("User does not have the permission to delete the data.");
    };

    let result: Result.Result<Data, Text> = store.del(key);

    switch (result) {
        case (#ok value) {};
        case (#err error) {
            throw Error.reject("Data cannot be deleted: " # error);
        };
    };
  };

  /**
   * Canister mgmt
   */

  // TODO: inter-canister call secure caller === manager canister !!!
  // Or as only controllers can execute following is enough security?

  public shared({ caller }) func transferCycles(): async() {
      await walletUtils.transferCycles(caller);
  };

  system func preupgrade() {
      entries := Iter.toArray(store.preupgrade().entries());
  };

  system func postupgrade() {
      store.postupgrade(entries);
      entries := [];
  };

};
