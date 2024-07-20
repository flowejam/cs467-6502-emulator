import pandas as pd


df = pd.read_csv('opcodes-list.csv')
#print(df.head())

#print(df.info())
#print(df.describe())

#print(f"df shape: {df.shape}")
all_ops_list = df['opcodes'].to_list()


unique_df = df.drop_duplicates()
#print(f" unique df shape: {unique_df.shape}")

unique_list = unique_df['opcodes'].to_list()
#print(unique_list)

# obtain the actual opcodes that are available from the 6502
target_ops_df = pd.read_csv('6502ops.csv')
#print(target_ops_df.head())
tops_list = target_ops_df['6502ops'].to_list()
#print(tops_list)


for i in range(len(tops_list)):
    tops_list[i] = tops_list[i][1:].rstrip()
    if len(tops_list[i]) == 1:
        tops_list[i] = f"0{tops_list[i][0]}"
    # if a character is alphabetical, then capitalize
    tops_list[i] = tops_list[i].upper()


target_list = []
# iterate over strings in unique_list and if they are present in tops_list, place in new array
for op in unique_list:
    if op in tops_list:
        target_list.append(op)

print(target_list)
#print(len(target_list))







