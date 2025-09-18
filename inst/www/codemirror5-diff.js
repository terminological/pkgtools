// --- Global variable to hold the MergeView instance ---
let globalMergeView;

// --- Function to initialize the MergeView based on data from Shiny ---
function initializeMergeView(data) {
  
  console.log('Shiny requested MergeView initialization with ', data);
  const container = document.getElementById('merge-view-container');
  
  // Create individual state configs for each pane
  const config = {
    value: data.value || "",
    origLeft: data.origLeft,
    origRight: data.origRight,
    mode: data.mode,
    lineNumbers: true,
    highlightDifferences: true,
    collapseIdentical: true
  };

  try {
    var target = document.getElementById("merge-view-container");
    globalMergeView = CodeMirror.MergeView(target, config)
    globalMergeView.wrap.style.height = "100%";
    if (globalMergeView.leftOriginal()) globalMergeView.leftOriginal().setSize(null, "100%");
    globalMergeView.editor().setSize(null, "100%");
    if (globalMergeView.rightOriginal()) globalMergeView.rightOriginal().setSize(null, "100%");
    resize(globalMergeView)
  } catch (error) {
     console.error('Error creating MergeView:', error);
     Shiny.setInputValue('merged_content_error', 'MergeView creation error: ' + error.message);
  }
}

// --- Function to get merged content and send it back to Shiny ---
function sendMergedContent() {
  let content = '';
  try {
    content = globalMergeView.editor().getValue()
    Shiny.setInputValue('merged_content', content);
  } catch (e) {
    console.error('Error retrieving merged content:', e);
    Shiny.setInputValue('merged_content_error', 'Could not retrieve editor content: ' + error.message);
  }
}

// --- Shiny Message Handlers ---
Shiny.addCustomMessageHandler('initialize_mergeview', function(message) {
   initializeMergeView(message);
});

Shiny.addCustomMessageHandler('request_merged_content', function(message) {
   sendMergedContent();
});


function mergeViewHeight(mergeView) {
  function editorHeight(editor) {
    if (!editor) return 0;
    return editor.getScrollInfo().height;
  }
  return Math.max(editorHeight(mergeView.leftOriginal()),
                  editorHeight(mergeView.editor()),
                  editorHeight(mergeView.rightOriginal()));
}

function resize(mergeView) {
  var height = mergeViewHeight(mergeView);
  for(;;) {
    if (mergeView.leftOriginal())
      mergeView.leftOriginal().setSize(null, height);
    mergeView.editor().setSize(null, height);
    if (mergeView.rightOriginal())
      mergeView.rightOriginal().setSize(null, height);

    var newHeight = mergeViewHeight(mergeView);
    if (newHeight >= height) break;
    else height = newHeight;
  }
  mergeView.wrap.style.height = height + "px";
}
