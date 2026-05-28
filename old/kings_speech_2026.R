# # manually adding announced titles from Kings speech
# kings_bills <- data.frame(
#   announced_bill_title = c(
#     "Civil Aviation Bill",
#     "Clean Water Bill",
#     "Commonhold and Leasehold Reform Bill",
#     "Competition Reform Bill",
#     "Digital Access to Services Bill",
#     "Education for All Bill",
#     "Electricity Generator Levy Bill",
#     "Energy Independence Bill",
#     "Enhancing Financial Services Bill",
#     "European Partnership Bill",
#     "Highways (Financing) Bill",
#     "Immigration and Asylum Bill",
#     "National Security Bill",
#     "Health Bill",
#     "Nuclear Regulation Bill",
#     "Overnight Visitor Levy Bill",
#     "Police Reform Bill",
#     "Regulating for Growth Bill",
#     "Remediation Bill",
#     "Removal of Peerages Bill",
#     "Small Business Protections (Late Payments) Bill",
#     "Social Housing Bill",
#     "Sovereign Grant Bill",
#     "Sporting Events Bill",
#     "Steel Industry (Nationalisation) Bill",
#     "Tackling State Threats Bill",
#     "Armed Forces Bill*",
#     "Courts and Tribunals Bill*",
#     "Cyber Security and Resilience (Network and Information Systems) Bill*",
#     "Northern Ireland Troubles Bill*",
#     "High Speed Rail (Crewe - Manchester) Bill / Northern Powerhouse Rail*",
#     "Public Office (Accountability) Bill*",
#     "Railways Bill*",
#     "Representation of the People Bill*"
#   ),
#   bill_id =c(
#     4125,
#     2,
#     3,
#     4,
#     5,
#     6,
#     7,
#     8,
#     9,
#     10,
#     11,
#     12,
#     13,
#     14,
#     15,
#     16,
#     17,
#     18,
#     19,
#     20,
#     21,
#     4126,
#     23,
#     4127,
#     4123,
#     26,
#     4065,
#     4083,
#     4035,
#     4022,
#     3094,
#     4019,
#     4030,
#     4080
#   )
# )


kings_bills <- data.frame(
  announced_bill_title = c(
    "Civil Aviation Bill",
    "Clean Water Bill",
    "Commonhold and Leasehold Reform Bill",
    "Competition Reform Bill",
    "Digital Access to Services Bill",
    "Education for All Bill",
    "Electricity Generator Levy Bill",
    "Energy Independence Bill",
    "Enhancing Financial Services Bill",
    "European Partnership Bill",
    "Highways (Financing) Bill",
    "Immigration and Asylum Bill",
    "National Security Bill",
    "Health Bill",
    "Nuclear Regulation Bill",
    "Overnight Visitor Levy Bill",
    "Police Reform Bill",
    "Regulating for Growth Bill",
    "Remediation Bill",
    "Removal of Peerages Bill",
    "Small Business Protections (Late Payments) Bill",
    "Social Housing Bill",
    "Sovereign Grant Bill",
    "Sporting Events Bill",
    "Steel Industry (Nationalisation) Bill",
    "Tackling State Threats Bill",
    "Armed Forces Bill*",
    "Courts and Tribunals Bill*",
    "Cyber Security and Resilience (Network and Information Systems) Bill*",
    "Northern Ireland Troubles Bill*",
    "High Speed Rail (Crewe - Manchester) Bill / Northern Powerhouse Rail*",
    "Public Office (Accountability) Bill*",
    "Railways Bill*",
    "Representation of the People Bill*"
  ),
  bill_id = c(
    4125,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    4126,
    0,
    4127,
    4123,
    0,
    4065,
    4083,
    4035,
    4022,
    3094,
    4019,
    4030,
    4080
  )
)


kings_bills_ids <- kings_bills$bill_id

# fetching all bills data
bills_url <- "https://bills-api.parliament.uk/api/v1/Bills?Take=10000"

bills_response <- GET(
  bills_url,
  add_headers(accept = "text/plain")
)

# tidying up response
bills_text <- content(bills_response, as = "text", encoding = "UTF-8")
bills_json <- fromJSON(bills_text)

all_bills <- bills_json$items |> 
  tibble()

# extracting list of bill ID's
bill_ids <- all_bills$billId

# adding ID's to table
table <- kings_bills |>
  left_join(all_bills, by = c("bill_id" = "billId"))


# Filter to King's Speech bills using bill ID
kings_ids <- c(3733, 3881, 3734, 3938) # include all relevant bill IDs here
kings_bills <- all_bills |> filter (billId %in% kings_ids)

