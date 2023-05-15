//import UIKit
import SimpleXChat

let s =
"""
{
    "resp": {
        "chatItemReaction": {
            "user": {
                "userId": 1,
                "agentUserId": "1",
                "userContactId": 1,
                "localDisplayName": "esp",
                "profile": {
                    "profileId": 1,
                    "displayName": "esp",
                    "fullName": "Evgeny @ SimpleX Chat",
                    "image": "",
                    "preferences": {
                        "fullDelete": {
                            "allow": "no"
                        },
                        "voice": {
                            "allow": "yes"
                        }
                    },
                    "localAlias": ""
                },
                "fullPreferences": {
                    "timedMessages": {
                        "allow": "yes"
                    },
                    "fullDelete": {
                        "allow": "no"
                    },
                    "reactions": {
                        "allow": "yes"
                    },
                    "voice": {
                        "allow": "yes"
                    },
                    "calls": {
                        "allow": "yes"
                    }
                },
                "activeUser": true,
                "showNtfs": true
            },
            "reaction": {
                "chatInfo": {
                    "direct": {
                        "contact": {
                            "contactId": 107,
                            "localDisplayName": "ep_2",
                            "profile": {
                                "profileId": 261,
                                "displayName": "ep",
                                "fullName": "Evgeny",
                                "image": "",
                                "localAlias": "My terminal"
                            },
                            "activeConn": {
                                "connId": 3112,
                                "agentConnId": "UmZETGpsLXJqZlFKQ1YwTg==",
                                "connLevel": 1,
                                "viaContact": 467,
                                "viaGroupLink": false,
                                "connType": "contact",
                                "connStatus": "ready",
                                "localAlias": "",
                                "entityId": 107,
                                "authErrCounter": 0,
                                "createdAt": "2022-10-29T13:53:00.860121Z"
                            },
                            "contactUsed": true,
                            "chatSettings": {
                                "enableNtfs": true
                            },
                            "userPreferences": {
                                "timedMessages": {
                                    "allow": "yes"
                                },
                                "calls": {
                                    "allow": "always"
                                }
                            },
                            "mergedPreferences": {
                                "timedMessages": {
                                    "enabled": {
                                        "forUser": true,
                                        "forContact": true
                                    },
                                    "userPreference": {
                                        "contact": {
                                            "preference": {
                                                "allow": "yes"
                                            }
                                        }
                                    },
                                    "contactPreference": {
                                        "allow": "yes"
                                    }
                                },
                                "fullDelete": {
                                    "enabled": {
                                        "forUser": false,
                                        "forContact": false
                                    },
                                    "userPreference": {
                                        "user": {
                                            "preference": {
                                                "allow": "no"
                                            }
                                        }
                                    },
                                    "contactPreference": {
                                        "allow": "no"
                                    }
                                },
                                "reactions": {
                                    "enabled": {
                                        "forUser": true,
                                        "forContact": true
                                    },
                                    "userPreference": {
                                        "user": {
                                            "preference": {
                                                "allow": "yes"
                                            }
                                        }
                                    },
                                    "contactPreference": {
                                        "allow": "yes"
                                    }
                                },
                                "voice": {
                                    "enabled": {
                                        "forUser": true,
                                        "forContact": true
                                    },
                                    "userPreference": {
                                        "user": {
                                            "preference": {
                                                "allow": "yes"
                                            }
                                        }
                                    },
                                    "contactPreference": {
                                        "allow": "yes"
                                    }
                                },
                                "calls": {
                                    "enabled": {
                                        "forUser": true,
                                        "forContact": true
                                    },
                                    "userPreference": {
                                        "contact": {
                                            "preference": {
                                                "allow": "always"
                                            }
                                        }
                                    },
                                    "contactPreference": {
                                        "allow": "yes"
                                    }
                                }
                            },
                            "createdAt": "2022-03-16T16:42:11.187499Z",
                            "updatedAt": "2023-05-15T19:18:57.610028Z",
                            "chatTs": "2023-05-15T19:34:14.774002Z"
                        }
                    }
                },
                "chatReaction": {
                    "chatDir": {
                        "directRcv": {}
                    },
                    "chatItem": {
                        "chatDir": {
                            "directRcv": {}
                        },
                        "meta": {
                            "itemId": 224886,
                            "itemTs": "2023-05-15T19:34:15Z",
                            "itemText": "hello there",
                            "itemStatus": {
                                "rcvRead": {}
                            },
                            "itemSharedMsgId": "MWhVT0xyNWg1N1FMZzVMRQ==",
                            "itemDeleted": null,
                            "itemEdited": false,
                            "itemTimed": null,
                            "itemLive": null,
                            "editable": false,
                            "localItemTs": "2023-05-15T20:34:15+01:00",
                            "createdAt": "2023-05-15T19:34:14.774002Z",
                            "updatedAt": "2023-05-15T19:34:17.650582Z"
                        },
                        "content": {
                            "rcvMsgContent": {
                                "msgContent": {
                                    "type": "text",
                                    "text": "hello there"
                                }
                            }
                        },
                        "reactions": [
                            {
                                "reaction": {
                                    "type": "emoji",
                                    "emoji": "❤"
                                },
                                "userReacted": true,
                                "totalReacted": 2
                            }
                        ]
                    },
                    "sentAt": "2023-05-15T19:37:28Z",
                    "reaction": {
                        "type": "emoji",
                        "emoji": "❤"
                    }
                }
            },
            "added": true
        }
    }
}
"""
//let s = "\"2022-04-24T11:59:23.703162Z\""
let json = getJSONDecoder()
let d = s.data(using: .utf8)!
print (try! json.decode(APIResponse.self, from: d))
