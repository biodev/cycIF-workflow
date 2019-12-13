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


from scipy import signal

import plotly.figure_factory as ff
import plotly
import plotly.graph_objs as go
from plotly.offline import download_plotlyjs, init_notebook_mode, plot, iplot

def compare_headers(expected, actual, name):
    missing_actual = np.setdiff1d(expected, actual)
    extra_actual = np.setdiff1d(actual, expected)
    if len(missing_actual) > 0:
        #print("WARNING: File '" + name + "' lacks the following expected header(s) after import header reformatting: \n" 
        #      + str(missing_actual))
        print("WARNING: File '" + name + "' lacks the following expected item(s): \n" + str(missing_actual))
    if len(extra_actual) > 0:
        #print("WARNING: '" + name + "' has the following unexpected header(s) after import header reformatting: \n" 
        #      + str(extra_actual))
        print("WARNING: '" + name + "' has the following unexpected item(s): \n" + str(extra_actual))
    
    return None



"""
This function plots distributions. It takes in a string title (title), a list of
dataframes from which to plot (dfs), a list of dataframe names for the legend
(names), a list of the desired colors for the plotted samples (colors),
a string for the x-axis label (x_label), ```a float binwidth for histrogram (bin_size)```,
a boolean to show the legend or not (legend),
and the names of the marker(s) to plot (input_labels). If not specified,
the function will plot all markers in one plot. input_labels can either be a 
single string, e.g., 'my_marker', or a list, e.g., ['my_marker1','my_marker2'].

The function will create a distribution plot and save it to png. It requires 
a list of items not to be considered as markers when evaluating column names
(not_markers) to be in memory. It also requires a desired output location of
the files (output_dir) to already be in memory. 
"""



def make_distr_plot_per_sample(title, location, dfs, df_names, colors, 
    x_label, legend, 
    xlims = None, markers = ['all'],
    histnorm = "",
    not_intensities = None,
    bin_size = 1):
    ### GET LIST OF MARKERS TO PLOT ###
    # Get list of markers to plot if not specified by user, using columns in first df
    # Writing function(parameter = FILLER) makes that parameter optional when user calls function,
    # since it is given a default value!
    if markers == ["all"]:
        markers = [c for c in dfs[0].columns.values if c not in not_intensities]
    elif not isinstance(markers, list):
        markers = [markers]
    # Make input labels a set to get only unique values, then put back into list
    markers = list(set(markers))
    
    ### GET XLIMS ###
    if xlims == None:
        mins = [df.loc[:,markers].min().min() for df in dfs]
        maxes = [df.loc[:,markers].max().max() for df in dfs]
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
        print("No markers plotted - all were singular value. Names and markers were " + str(df_names) + ", " + str(markers))
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
        plot_bgcolor = 'white', showlegend = legend)#, legend_x = 3)
    # Adjust opacity
    fig.update_traces(opacity=0.6)
    # Adjust x-axis parameters
    fig.update_xaxes(title_text = x_label, showline=True, linewidth=2, linecolor='black', 
        tickfont=dict(size=18), range = xlims) # x lims was here
    # Adjust y-axis parameters
    fig.update_yaxes(title_text = ylabel,showline=True, linewidth=1, linecolor='black',
        tickfont=dict(size=18))
    

    ### SAVE/DISPLAY PLOT ###
    # Save plot to HTML
    # plotly.io.write_html(fig, file = output_dir + "/" + title + ".html")
    # Plot in new tab
    #plot(fig)   
    # Save to png
    filename = os.path.join(location, title.replace(" ","_") + ".png")
    fig.write_image(filename)
    return None





    # this could be changed to use recursion and make it 'smarter'

def shorten_feature_names(long_names):
    name_dict = dict(zip(long_names,[n.split('_')[0] for n in long_names]))
    names_lts, long_names, iteration = shorten_feature_names_helper(name_dict, long_names, 1)
    # names_lts = names long-to-short
    # names_stl = names stl
    names_stl = {}
    for n in names_lts.items():
        names_stl[n[1]] = n[0]
    return names_lts, names_stl
    

def shorten_feature_names_helper(name_dict, long_names, iteration):
    #print("\nThis is iteration #"+str(iteration))
    #print("name_dict is: " + str(name_dict))
    #print("long_names is: " + str(long_names))
    ## If the number of unique nicknames == number of long names
    ## then the work here is done
    #print('\nCompare lengths: ' + str(len(set(name_dict.values()))) + ", " + str(len(long_names)))
    #print('set(name_dict.values()): ' + str(set(name_dict.values())))
    #print('long_names: ' + str(long_names))
    if len(set(name_dict.values())) == len(long_names): 
        #print('All done!')
        return name_dict, long_names, iteration
    
    ## otherwise, if the number of unique nicknames is not
    ## equal to the number of long names (must be shorter than),
    ## then we need to find more unique names
    iteration += 1
    nicknames_set = set()
    non_unique_nicknames = set()
    # construct set of current nicknames
    for long_name in long_names:
        #print('long_name is ' + long_name + ' and non_unique_nicknames set is ' + str(non_unique_nicknames))
        short_name = name_dict[long_name]
        if short_name in nicknames_set:
            non_unique_nicknames.add(short_name)
        else:
            nicknames_set.add(short_name)
    #print('non_unique_nicknames are: ' + str(non_unique_nicknames))
    
    # figure out all long names associated
    # with the non-unique short names
    trouble_long_names = set()
    for long_name in long_names:
        short_name = name_dict[long_name]
        if short_name in non_unique_nicknames:
            trouble_long_names.add(long_name)
    
    #print('troublesome long names are: ' + str(trouble_long_names))
    #print('name_dict: ' + str(name_dict))
    # operate on all names that are associated with
    # the non-unique short nicknames
    for long_name in trouble_long_names:
        #print('trouble long name is: ' + long_name)
        #print('old nickname is: ' + name_dict[long_name])
        name_dict[long_name] = '_'.join(long_name.split('_')[0:iteration])
        #print('new nickname is: ' + name_dict[long_name])
    shorten_feature_names_helper(name_dict, long_names, iteration)
    return name_dict, long_names, iteration


# First get the number of lines corresponding to each sample
# then divid by the total number of lines to get proportions
# then multiply by the deisred number of rows. 
# We will then round this number to the nearest integer.
def maintain_value_counts(df, col, count):
    values = df[col].value_counts().sort_index()
    props = values/df.shape[0]
    desired_rows = (props*count).round().astype(int)
    counts_df = pd.DataFrame({'current_row_count':values,'prop':props,'desired_row_count':desired_rows})
    return counts_df

def create_equal_value_counts(df, col, count):
    values = df[col].value_counts().sort_index()
    props = values/df.shape[0]
    desired_rows = round((count / len(df[col].unique())))
    counts_df = pd.DataFrame({'current_row_count':values,'prop':props})
    counts_df['desired_row_count'] = counts_df.apply(lambda row: min(desired_rows, row['current_row_count']), axis = 1)
    return counts_df

def heatmap_function(title,
            data,
              method, metric, cmap,
              cbar_kws, xticklabels, save_loc,
              row_cluster, col_cluster, 
            annotations = {'rows':[],'cols':[]}):
    
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
    g = sb.clustermap(data = data, 
                  robust = True,
                  method = method, metric = metric,
                  cmap = cmap,
                  row_cluster = row_cluster, col_cluster = col_cluster,
                  figsize = (40,30),
                  row_colors=row_mappings, col_colors=col_mappings,
                      yticklabels = False,
                     cbar_kws = cbar_kws,
                     xticklabels = xticklabels)

    # Add title
    g.fig.suptitle(title, fontsize = 60.0)
    
    #And now for the legends:
    # iterate through 'rows', 'cols'
    for ann_type in sorted(annotations.keys()):
        # iterate through each individual annotation feature
        for ann in annotations[ann_type]:
            color_dict = ann['dict']
            handles = []
            for item in sorted(color_dict.keys()):
                h = g.ax_col_dendrogram.bar(0,0, color = color_dict[item], label = item,
                                           linewidth = 0)
                handles.append(h)
            legend = plt.legend(handles = handles, loc = ann['location'], title = ann['label'],
                               bbox_to_anchor=ann['bbox_to_anchor'],
                               bbox_transform=plt.gcf().transFigure)
            ax = plt.gca().add_artist(legend)

    # Save image
    filename = os.path.join(save_loc, title.lower().replace(" ","_") + ".png")
    g.savefig(filename)
    
    return None


    
# sources - 
#https://stackoverflow.com/questions/27988846/how-to-express-classes-on-the-axis-of-a-heatmap-in-seaborn
# https://matplotlib.org/3.1.1/tutorials/intermediate/legend_guide.html


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


def rgb_tuple_from_str(rgb_str):
    rgb_str = rgb_str.replace("(","").replace(")","").replace(" ","")
    rgb = list(map(float,rgb_str.split(",")))
    return tuple(rgb)

def color_dict_to_df(cd, column_name):
    df = pd.DataFrame.from_dict(cd, orient = 'index')
    df['rgb'] = df.apply(lambda row: (np.float64(row[0]), np.float(row[1]), np.float64(row[2])), axis = 1)
    df = df.drop(columns = [0,1,2])
    df['hex'] = df.apply(lambda row: mplc.to_hex(row['rgb']), axis = 1)
    df[column_name] = df.index
    return df