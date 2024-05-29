module Fixture (expectedFamilies) where

import Lib (JsonValue (..))

expectedFamilies :: JsonValue
expectedFamilies =
  JsonObject
    [ ( "families",
        JsonArray
          [ JsonObject
              [ ("familyName", JsonString "Smith"),
                ( "members",
                  JsonArray
                    [ JsonObject
                        [ ("name", JsonString "John"),
                          ("surname", JsonString "Smith"),
                          ("age", JsonNumber 40),
                          ("eyeColor", JsonString "blue")
                        ],
                      JsonObject
                        [ ("name", JsonString "Jane"),
                          ("surname", JsonString "Smith"),
                          ("age", JsonNumber 38),
                          ("eyeColor", JsonString "green")
                        ],
                      JsonObject
                        [ ("name", JsonString "Jake"),
                          ("surname", JsonString "Smith"),
                          ("age", JsonNumber 10),
                          ("eyeColor", JsonString "blue")
                        ]
                    ]
                )
              ],
            JsonObject
              [ ("familyName", JsonString "Doe"),
                ( "members",
                  JsonArray
                    [ JsonObject
                        [ ("name", JsonString "John"),
                          ("surname", JsonString "Doe"),
                          ("age", JsonNumber 45),
                          ("eyeColor", JsonString "brown")
                        ],
                      JsonObject
                        [ ("name", JsonString "Jane"),
                          ("surname", JsonString "Doe"),
                          ("age", JsonNumber 42),
                          ("eyeColor", JsonString "hazel")
                        ],
                      JsonObject
                        [ ("name", JsonString "Judy"),
                          ("surname", JsonString "Doe"),
                          ("age", JsonNumber 12),
                          ("eyeColor", JsonString "brown")
                        ]
                    ]
                )
              ]
          ]
      )
    ]
