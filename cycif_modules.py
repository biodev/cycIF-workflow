# CycIF Modules
#
# Megan Grout groutm2020@alumni.ohsu.edu
#
# These functions were developed for use in the CycIF workflow,
# which was developed based off of the scripts written by
# Dr. Marilyne Labrie and Nick Kendsersky.
#
# Last updated: 20200527

# Import neccessary libraries
import os
import numpy as np
import pandas as pd
import subprocess
import os
import random
import re
import pandas as pd
import numpy as np
import seaborn as sb
import matplotlib.pyplot as plt
import matplotlib.colors as mplc
import subprocess


#from scipy import signal

# Plotly-related libraries
import plotly.figure_factory as ff
import plotly
import plotly.graph_objs as go
from plotly.offline import download_plotlyjs, init_notebook_mode, plot, iplot


"""
This function compares two lists of values and returns which values are present in one and not the
other and vice versa by comparing them. It takes in a string "name" identifier and an object 
"actual" object corresponding to the actual headers in a dataframe. "expected" is an object 
corresponding to the header values we expect to see. Results are printed to console and nothing is 
returned.
"""
def compare_headers(expected, actual, name):
    missing_actual = np.setdiff1d(expected, actual)
    extra_actual = np.setdiff1d(actual, expected)
    if len(missing_actual) > 0:
        print("WARNING: File '" + name + "' lacks the following expected item(s): \n" 
        	+ str(missing_actual))
    if len(extra_actual) > 0:
        print("WARNING: '" + name + "' has the following unexpected item(s): \n" 
        	+ str(extra_actual))
    
    return None



"""
This function will create a distribution plot and save it as a png file. It takes in: a string title
(title); a string location to save the file (location); a list of dataframes from which to plot 
(dfs); a list of dataframe names for the legend (names); a list of the desired colors for the 
plotted samples in (r, g, b) format where r, g, b are floats less than 1.0. (colors); a string for
the x-axis label (x_label); a boolean to show the legend or not (legend); an optional list of two 
numbers to set the x-axies limits (xlims); an optional list of all features in the input dfs to plot
(markers), which much be present in all dfs; an optional string to signify the type of distribution
plot (histnorm); an optiona list of non-intensity feature from the df to plot (not_intensities); and
an optional number to specify the plotted bin width (bin_size). The default markers value will plot
all features in the input dfs. The default string of "" for histnorm will plot counts; "probability"
and "percent" are also options per Plotly docs. The default not_intensities value of None will 
exclude no features from plotting when markers is not specified by the user. The default bin width 
is 1.
"""
def make_distr_plot_per_sample(title, location, dfs, df_names, colors, 
    x_label, legend, 
    xlims = None, markers = ['all'],
    histnorm = "",
    not_intensities = None,
    bin_size = 1):
    ### GET LIST OF MARKERS TO PLOT ###
    # Get list of markers to plot if not specified by user, using columns in first 
    # df in dfs
    # Writing function using "parameter = default_value" makes that parameter optional
    # when user calls function, since it already has a default value!
    if markers == ["all"]:
        markers = [c for c in dfs[0].columns.values if c not in not_intensities]
    # If the user input a single marker name as a string, make it a list
    elif not isinstance(markers, list):
        markers = [markers]
    # Make input labels a set to get only unique values, then put back into list
    markers = list(set(markers))
    
    ### GET XLIMS ###
    # If not default value
    if xlims == None:
    	# Find the minimum value in each df
        mins = [df.loc[:,markers].min().min() for df in dfs]
        # Find the maximum value in each dfs
        maxes = [df.loc[:,markers].max().max() for df in dfs]
        # Find the minimum and maximum values overall
        xlims = [min(mins), max(maxes)]
    if not isinstance(xlims, list):
        print("Problem - xlmis not list. Exiting method...")
        return None
    ### GET YLABEL ###
    if histnorm == "":
    	ylabel = "Count"
    else:
    	ylabel = histnorm
    ### CHECK DATA CAN BE PLOTTED ###
    # Check for data with only 1 unique value - this will cause error if plotted
    group_labels = []
    hist_data = []
    # Iterate through all dataframes (dfs)
    for i in range(len(dfs)):
        # Iterate through all marker labels
        for f in markers:
            # If there is only one unique value in the marker data for this dataframe,
            # you cannot plot a distribution plot. It gives you a linear algebra
            # singular value matrix error
            if dfs[i][f].nunique() != 1:
                # Add df name and marker name to labels list
                # If we have >1 df, we want to make clear
                # which legend label is associated with which df
                if len(df_names) > 1:
                    group_labels.append(df_names[i]+"_"+f)
                else:
                    group_labels.append(f)
                # add the data to the data list
                hist_data.append(dfs[i][f])
    # if no data had >1 unique values, there is nothing to plot
    if len(group_labels) < 1:
        print("No markers plotted - all were singular value. Names and markers were " + 
        	str(df_names) + ", " + str(markers))
        return None

    ### TRANSFORM COLOR ITEMS TO CORRECT TYPE ###
    if isinstance(colors[0], tuple):
        colors = ['rgb' + str(color) for color in colors]
    
    ### PLOT DATA ###
    # Create plot
    fig = ff.create_distplot(hist_data, group_labels, 
        #colors=colors, bin_size=bin_size,  show_rug=False)#show_hist=False,
        colors=colors, show_rug=False, histnorm=histnorm, bin_size = bin_size)
    # Adjust title, font, background color, legend...
    fig.update_layout(title_text=title, font=dict(size=18), 
        plot_bgcolor = 'white', showlegend = legend)
    # Adjust opacity
    fig.update_traces(opacity=0.6)
    # Adjust x-axis parameters
    fig.update_xaxes(title_text = x_label, showline=True, linewidth=2, linecolor='black', 
        tickfont=dict(size=18), range = xlims)
    # Adjust y-axis parameters
    fig.update_yaxes(title_text = ylabel,showline=True, linewidth=1, linecolor='black',
        tickfont=dict(size=18))

    ### SAVE/DISPLAY PLOT ###
    # Save plot to HTML
    # Plot in new tab
    #plot(fig)   
    # Save to png
    filename = os.path.join(location, title.replace(" ","_") + ".png")
    fig.write_image(filename)
    return None



## These two functions are used to derive the full column names' shortened nicknames. It uses a 
## concept called recursion, in which a function will operate on its input, and, permitting certain
## conditions, use its output as input in anoter iteration of itself. Here, the user will call 
## 'shorten_feature_names' once, which will preprocess the input for the repeated 
## 'shorten_feature_names_helper' calls, which will occur in series until a unique set of shortened
## column names has been reached. In each iteration, the helper function will extend the 
## abbreviation of the non-unique nicknames include another underscore-separated segment

"""
This function takes in the column headers for a dataframe (long_names) and returns two dictionaries:
one where the keys are the values from long_names and the values are the corresponding unique 
nicknames; and the other, where the keys are the unique nicknames and the values are are the
corresponding original header names.
"""
def shorten_feature_names(long_names):
	# Create a dictionary of all long names where the value is
	# each long name, and the key is a list of the first 'chunks' that composes that name, when the
	# name was split on underscores (or spaces turned to underscores)
    name_dict = dict(
    	zip(long_names,[n.replace(" ","_").split('_')[0] for n in long_names]))
    # Call the helper recursion function one time. The outputs are the long-to-short name
    # dictionary, the long (original) names, and the iteration (number of times the helper function
    # was)
    names_lts, long_names, iteration = \
    	shorten_feature_names_helper(name_dict, long_names, 1)
    
    # Create a dictionary to hold the short-to-long name conversions, and populate it
    names_stl = {}
    for n in names_lts.items():
        names_stl[n[1]] = n[0]
    return names_lts, names_stl
    
"""
This function takes in a dictionary of names where the values are the original string names, and the
values are the strings 
"""
def shorten_feature_names_helper(name_dict, long_names, iteration):
	# If the number of long names is equal to the number of unique nicknames, then no need to 
	# process further
    if len(set(name_dict.values())) == len(long_names): 
        return name_dict, long_names, iteration
    
    # Otherwise, if the number of unique nicknames is not equal to the number of long names 
    # (must be shorter than), then we need to find more unique names
    # increment iteration, will be used to get next 'chunk' in long names
    iteration += 1
    nicknames_set = set()
    non_unique_nicknames = set()
    # construct set of current nicknames
    # Get the current nickname
    for long_name in long_names:
        short_name = name_dict[long_name]
        # If the short_name is already in nicknames_set, that means it is a non-unique nickname
        # and we will record that accordingly
        if short_name in nicknames_set:
            non_unique_nicknames.add(short_name)
        # we have not yet seen this nickname
        else:
            nicknames_set.add(short_name)
   
    # Figure out ALL long names associated with the non-unique short names, since these are the
    # long names we will need to make new nicknames for. 
    trouble_long_names = set()
    # iterate through all long names
    for long_name in long_names:
    	# find nickname
        short_name = name_dict[long_name]
        # check if this nickname is in set of nicknames we know are non-unique
        if short_name in non_unique_nicknames:
        	# Record this long name that yields a non-unique short name
            trouble_long_names.add(long_name)
    
    # Operate on all names that are associated with the non-unique short nicknames
    for long_name in trouble_long_names:
    	# Set the nickname in name_dict to a new nickname
    	# The new nickname is the long name, split by underscores (or spaces replaced with 
    	# underscores), where a number of chunks--corresponding to the iteration value--are joined
    	# together to create a new name, separated by underscores. For example, if the long name
    	# was my_long_name_is_this, and iteration was 3, the string would be my_long_time.
        name_dict[long_name] = '_'.join(long_name.replace(" ","").split('_')[0:iteration])
    shorten_feature_names_helper(name_dict, long_names, iteration)
    return name_dict, long_names, iteration

## The following three functions are used to create a subset dataframe, usually for the purpose of
## running code faster, where a feature is selected (usually Sample_ID), and the contribution of the
## unique values to that df are evaluated. A new subset df, with user-defined length, either has 
## the same proprotions of unique values in the specified feature, or even proprotions, as defined
## by the user.

"""
This function takes in a pandas dataframe (df), a string that is a header name in the df (col), and
an integer (count) of the desired length of the returned dataframe. The function will return a 
dataframe of value counts of the unqiue values in df[col] as index, the 'current_row_count' column
of the number of rows corresponding to each uniqe df[col] value, 'prop' corresponding to the
proportion of each unique col's values rows in the oritinal dataframe, and 'desired_row_count',
which is the number of that unique col's value's rows in a new dataframe, where the proprotions
are the same as in the original df.
"""
def maintain_value_counts(df, col, count):
	# Values of each feature (counts)
    values = df[col].value_counts().sort_index()
    # Proportion of each unique col value in df
    props = values/df.shape[0]
    # Proportion of contributino of each unique col value, multiplied by the total number of lines
    # we want in a future df
    desired_rows = (props*count).round().astype(int)
    # Store and return data
    counts_df = pd.DataFrame(
    	{'current_row_count':values,'prop':props,'desired_row_count':desired_rows})
    return counts_df

"""
This function takes in a pandas dataframe (df), a string that is a header name in the df (col), and
an integer (count) of the desired length of the returned dataframe. The function will return a 
dataframe of value counts of the unqiue values in df[col] as index, the 'current_row_count' column
of the number of rows corresponding to each uniqe df[col] value, 'prop' corresponding to the
proportion of each unique col's values rows in the oritinal dataframe, and 'desired_row_count',
which is the number of that unique col's value's rows in a new dataframe, where the proprotions
are equal to each other.
"""
def create_equal_value_counts(df, col, count):
	# Values of each feature (counts)
    values = df[col].value_counts().sort_index()
    # Proportion of each unique col value in df
    props = values/df.shape[0]
    # We want ech unique col value wants to contribute the same number of rows in future df
    desired_rows = round((count / len(df[col].unique())))
    # Store and treturn data
    counts_df = pd.DataFrame({'current_row_count':values,'prop':props})
    # We do not want more rows for a given unique col value than exist
    counts_df['desired_row_count'] = counts_df.apply(
    	lambda row: min(desired_rows, row['current_row_count']), axis = 1)
    return counts_df

"""
This function takes in: a pandas dataframe (df); a string column of interest (col) in that df; an 
integer corresponding to the length of the returned df (count); and a string specifying ratio type
(ratio), which must be either 'equal' or 'original'. The funcition returns a dataframe where rows
of the input df were randomly sampled, without repalcement, to create a df of length counrt, where
the proportion of each unique col value is either equal or the same as it was originally.
"""
def create_subset(df, col, count, ratio):
	# Check for acceptable parameter
    if ratio not in ['equal','original']:
        print("'ratio' must be either 'equal' or 'original'")
        print('Exiting...')
        return None
    if count > df.shape[0]:
    	print(str(count) + " greater than dataframe length. Usuing df length (" + str(df.shape[0]) \
    	 + ") instead.")
    count = min(count, df.shape[0])
    # Generate helper df to gete expected row counts for each unique df[col] value
    if ratio == 'original':
        print('here')
        counts_df = maintain_value_counts(df, col, count)
    else:
        counts_df = create_equal_value_counts(df, col, count)
    # Create a dataframe to hold subset data
    subset_df = pd.DataFrame(columns = df.columns)
    # Iterate through all unique df[col] values, sample correct number of rows, and add them to
    # output df
    for c in df[col].unique():
        a = counts_df.loc[counts_df.index == c,'current_row_count'].values[0]
        size = int(counts_df.loc[counts_df.index == c,'desired_row_count'].values[0])
        random_rows = np.random.choice(a = a, size = size, replace = False)
        df_sample = df.loc[df[col] == c,:]
        subset_df = subset_df.append(df_sample.iloc[random_rows,:])
    return subset_df

"""
This function creates a seaborn heatmap and saves the figure as a png. If given annotatin data, will
plot row/column colors and create corresponding legends. It requies: a string title (title) for the
plot; a pandas dataframe of data to plot (data); a string method for plotting (method), e.g.,
'ward'; a string metric for plotting the distance between features, e.g., 'correlation', or 
'euclidean'; a string specifiy the colormape to be used (cmap); a dictionary of optional colorbar
keyword arguments (cbar_kws), e.g., for use in labeling colorbar scale legend; a list of labels for
the x-axis features (xticklabels); a string for the directory in which the file whould be saved
(save_loc); boolean values for row and column clustering (row_cluster, col_cluster); and an optional
dictionary containing annotations for row and column colors. If not provided in annotations, no data
will be plotted as row/column colors, and no accompanying legends will be produced.
"""
def heatmap_function(
	title, data, method, metric, cmap, cbar_kws, xticklabels, save_loc, row_cluster, col_cluster, 
            annotations = {'rows':[],'cols':[]}):
    
    # Set seaborn font scale
    sb.set(font_scale= 2.0)

    # Extract row and column mappings
    row_mappings = []
    col_mappings = []
    for ann in annotations['rows']:
        row_mappings.append(ann['mapping'])
    for ann in annotations['cols']:
        col_mappings.append(ann['mapping'])
    # If empty lists, convert to None so seaborn accepts
    # as the row_colors or col_colors objects
    if len(row_mappings) == 0:
        row_mappings = None
    if len(col_mappings) == 0:
        col_mappings = None

    # Create clustermap
    g = sb.clustermap(
    	data = data, robust = True, method = method, metric = metric, cmap = cmap, 
    	row_cluster = row_cluster, col_cluster = col_cluster, figsize = (40,30),
    	row_colors=row_mappings, col_colors=col_mappings, yticklabels = False, cbar_kws = cbar_kws,
    	xticklabels = xticklabels)

    # Add title
    g.fig.suptitle(title, fontsize = 60.0)
    
    #And now for the legends:
    # iterate through 'rows', 'cols'
    for ann_type in sorted(annotations.keys()):
        # iterate through each individual annotation feature
        for ann in annotations[ann_type]:
        	# Get the color dictionary
            color_dict = ann['dict']
            # Iterate through all keys in the color dictionary, create/capture handles for legend
            handles = []
            for item in sorted(color_dict.keys()):
                h = g.ax_col_dendrogram.bar(0,0, color = color_dict[item], label = item,
                                           linewidth = 0)
                handles.append(h)
            # Add legend to plot
            legend = plt.legend(handles = handles, loc = ann['location'], title = ann['label'],
                               bbox_to_anchor=ann['bbox_to_anchor'],
                               bbox_transform=plt.gcf().transFigure)
            ax = plt.gca().add_artist(legend)

    # Save image
    filename = os.path.join(save_loc, title.lower().replace(" ","_") + ".png")
    g.savefig(filename)
    return None

	# sources - 
	# https://stackoverflow.com/questions/27988846/how-to-express-classes-on-the-axis-of-a-heatmap-in-seaborn
	# https://matplotlib.org/3.1.1/tutorials/intermediate/legend_guide.html

"""
This function takes in a string filename, and an integer (lines_read). It uses subprocess to assess
the number of lines in filename. Worth mentioning since this is what failed on some machines (1 PC
failure; 1 PC + 1 MacBook success) and not others in development. If the identified number of lines
in the file differs from lines_read, then a warning is printed. Nothing is returned.
"""
def verify_line_no(filename, lines_read):
    # Use Linux "wc -l" command to get the number of lines in the unopened file
    wc = subprocess.check_output(['wc', '-l', filename]).decode("utf-8")
    # Take that string, turn it into a list, extract the first item,
    # and make that an int - this is the number of lines in the file
    wc = int(wc.split()[0])
    if lines_read != wc:
        print("WARNING: '" + filename + "' has " + str(wc) + 
            " lines, but imported dataframe has " 
              + str(lines_read) + " (including header).")
    return None


"""
This function takes in a string that corresponds to the rgb values for a color and rescues
the float tuple format. String must be string version of "(r, g, b)", where r, g, b, are floats less
than 1.0.
"""
def rgb_tuple_from_str(rgb_str):
    rgb_str = rgb_str.replace("(","").replace(")","").replace(" ","")
    rgb = list(map(float,rgb_str.split(",")))
    return tuple(rgb)

"""
This function takes in a dictionary cd and a string column_name. It creates a pandas DataFrame from
cd and returns a df with the rgb and hexadecmial values for the df.
"""
def color_dict_to_df(cd, column_name):
    df = pd.DataFrame.from_dict(cd, orient = 'index')
    df['rgb'] = df.apply(
    	lambda row: (np.float64(row[0]), np.float(row[1]), np.float64(row[2])), axis = 1)
    df = df.drop(columns = [0,1,2])
    df['hex'] = df.apply(lambda row: mplc.to_hex(row['rgb']), axis = 1)
    df[column_name] = df.index
    return df