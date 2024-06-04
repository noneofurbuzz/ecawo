import Text "mo:base/Text";
import Time "mo:base/Time";
import Principal "mo:base/Principal";
import HashMap "mo:base/HashMap";
import List "mo:base/List";
import Nat "mo:base/Nat";
import Hash "mo:base/Hash";
import Nat32 "mo:base/Nat32";
import Result "mo:base/Result";
import Bool "mo:base/Bool";

actor {

  type CampaignId = Nat32;
  func hashNat32(natty : Nat32) : Nat32 {
    return natty;
  };

  type Campaign = {
    title : Text;
    description : Text;
    funding_goal : Nat;
    created_at : Time.Time;
    end_date : ?Time.Time;
    creator : Principal;
  };

  type Donation = {
    id : Nat;
    donor : Principal;
    amount : Nat;
  };

  type CampaignCreator = {
    name : Text;
    about : Text;
  };

  type HiveError = {
    #AccountAlreadyExists;
    #AccountDoesNotExist;
    #CallerHasNoAccount;
    #UnauthorizedAsCampaignCreator;
    #CampaignDoesNotExist;
    #CampaignIsNotActive;
    #DonationDoesNotExist;
  };
  type HiveResult<T> = Result.Result<T, HiveError>;

  var creator_metadata : HashMap.HashMap<Principal, CampaignCreator> = HashMap.HashMap(10, Principal.equal, Principal.hash);
  var creator_campaigns : HashMap.HashMap<Principal, List.List<CampaignId>> = HashMap.HashMap(10, Principal.equal, Principal.hash);
  var campaign_metadata : HashMap.HashMap<CampaignId, Campaign> = HashMap.HashMap(10, Nat32.equal, hashNat32);
  var donations : HashMap.HashMap<CampaignId, List.List<Donation>> = HashMap.HashMap(10, Nat32.equal, hashNat32);
  var next_campaign_id : CampaignId = 0;
  var next_donation_id : Nat = 0;

  func principal_has_account(principal : Principal) : Bool {
    creator_metadata.get(principal) != null;
  };

  func principal_created_campaign(principal : Principal, campaign_id : CampaignId) : Bool {
    switch (creator_campaigns.get(principal)) {
      case (?campaigns) {
        for (campaign_id_ in List.toIter(campaigns)) {
          if (campaign_id_ == campaign_id) {
            return true;
          };
        };
      };
      case (null) {
        return false;
      };
    };
    return false;
  };

  func campaign_is_active(campaign_id : CampaignId) : Bool {
    switch (campaign_metadata.get(campaign_id)) {
      case (?metadata) {
        switch (metadata.end_date) {
          case (?end_date) {
            return Time.now() < end_date;
          };
          case (null) {
            return true;
          };
        };
      };
      case (null) {
        return false;
      };
    };
    return false;
  };

  func campaign_exists(campaign_id : CampaignId) : Bool {
    return campaign_metadata.get(campaign_id) != null;
  };

  public shared (messageObject) func create_campaign_creator_account(
    name : Text,
    about : Text,
  ) : async HiveResult<()> {
    let { caller } = messageObject;
    // Check if creator exists.
    if (principal_has_account(caller)) {
      return #err(#AccountAlreadyExists);
    };

    let metadata : CampaignCreator = { name = name; about = about };
    creator_metadata.put(caller, metadata);

    return #ok(());
  };

  public shared (messageObject) func create_campaign(
    title : Text,
    description : Text,
    funding_goal : Nat,
    end_date : Time.Time,
  ) : async HiveResult<{ id : CampaignId; metadata : Campaign }> {
    let { caller } = messageObject;
    // Check if creator exists.
    if (not principal_has_account(caller)) {
      return #err(#CallerHasNoAccount);
    };

    let campaign : Campaign = {
      title = title;
      description = description;
      funding_goal = funding_goal;
      created_at = Time.now();
      end_date = ?end_date;
      creator = caller;
    };

    // Get CampaignId
    let campaign_id : CampaignId = next_campaign_id;

    switch (creator_campaigns.get(caller)) {
      case (?existing_campaigns) {
        let new_campaigns_list = List.push(campaign_id, existing_campaigns);
        creator_campaigns.put(caller, new_campaigns_list);
      };
      case (null) {
        let new_campaigns_list = List.push(campaign_id, List.nil());
        creator_campaigns.put(caller, new_campaigns_list);
      };
    };
    campaign_metadata.put(campaign_id, campaign);
    next_campaign_id += 1;

    return #ok({ id = campaign_id; metadata = campaign });
  };

  public shared (messageObject) func close_campaign(
    campaign_id : CampaignId
  ) : async HiveResult<()> {
    let { caller } = messageObject;
    // Check if creator exists.
    if (creator_metadata.get(caller) == null) {
      return #err(#CallerHasNoAccount);
    };

    switch (creator_campaigns.get(caller)) {
      case (?campaigns) {
        var is_creator : Bool = false;
        for (campaign_id_ in List.toIter(campaigns)) {
          if (campaign_id_ == campaign_id) {
            is_creator := true;
          };
        };
      };
      case (null) {
        return #err(#CampaignDoesNotExist);
      };
    };

    switch (campaign_metadata.get(campaign_id)) {
      case (?metadata) {
        var metadata_ : Campaign = {
          title = metadata.title;
          description = metadata.description;
          funding_goal = metadata.funding_goal;
          created_at = metadata.created_at;
          end_date = ?Time.now();
          creator = metadata.creator;
        };
        campaign_metadata.put(campaign_id, metadata_);
      };
      case (null) {
        return #err(#CampaignDoesNotExist);
      };
    };

    return #ok(());
  };

  public func get_campaign_metadata(campaign_id : CampaignId) : async HiveResult<Campaign> {
    switch (campaign_metadata.get(campaign_id)) {
      case (?metadata) {
        return #ok(metadata);
      };
      case (null) {
        return #err(#CampaignDoesNotExist);
      };
    };
  };

  public shared (messageObject) func donate_to_campaign(
    campaign_id : CampaignId
  ) : async HiveResult<Donation> {
    let { caller } = messageObject;
    // Check if creator exists.
    if (creator_metadata.get(caller) == null) {
      return #err(#CallerHasNoAccount);
    };

    switch (campaign_metadata.get(campaign_id)) {
      case (?metadata) {
        let donation_id = next_donation_id;
        let donation : Donation = {
          id = donation_id;
          donor = caller;
          amount = 1;
        };
        switch (donations.get(campaign_id)) {
          case (?existing_donations) {
            let new_donations_list = List.push(donation, existing_donations);
            donations.put(campaign_id, new_donations_list);
          };
          case (null) {
            let new_donations_list = List.push(donation, List.nil());
            donations.put(campaign_id, new_donations_list);
          };
        };
        next_donation_id += 1;

        return #ok(donation);
      };
      case (null) {
        return #err(#CampaignDoesNotExist);
      };
    };
  };

  public shared (messageObject) func refund_donation(
    campaign_id : CampaignId
  ) : async HiveResult<()> {
    let { caller } = messageObject;
    // Check if creator exists.
    if (creator_metadata.get(caller) == null) {
      return #err(#CallerHasNoAccount);
    };

    switch (campaign_metadata.get(campaign_id)) {
      case (?metadata) {
        switch (donations.get(campaign_id)) {
          case (?existing_donations) {
            var new_donations_list : List.List<Donation> = List.nil();
            var donation_found : Bool = false;
            for (donation in List.toIter(existing_donations)) {
              if (donation.donor == caller) {
                // TODO: Refund
                donation_found := true;
              } else {
                new_donations_list := List.push(donation, new_donations_list);
              };
            };
            if (not donation_found) {
              return #err(#DonationDoesNotExist);
            };
            donations.put(campaign_id, new_donations_list);
          };
          case (null) {
            return #err(#AccountDoesNotExist);
          };
        };

        return #ok(());
      };
      case (null) {
        return #err(#CampaignDoesNotExist);
      };
    };
  };

  public shared (messageObject) func get_donation(
    donation_id : Nat,
    campaign_id : CampaignId,
  ) : async HiveResult<Donation> {
    let { caller } = messageObject;

    switch (campaign_metadata.get(campaign_id)) {
      case (?metadata) {
        switch (donations.get(campaign_id)) {
          case (?existing_donations) {
            for (donation in List.toIter(existing_donations)) {
              if (donation.id == donation_id) {
                return #ok(donation);
              };
            };
            return #err(#DonationDoesNotExist);
          };
          case (null) {
            return #err(#DonationDoesNotExist);
          };
        };
      };
      case (null) {
        return #err(#CampaignDoesNotExist);
      };
    };
  };

};
