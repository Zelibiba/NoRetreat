using Avalonia;
using Avalonia.Controls;
using Avalonia.Controls.Presenters;
using Avalonia.Input;
using Avalonia.Interactivity;
using Avalonia.Media.Transformation;
using Avalonia.Reactive;
using Avalonia.VisualTree;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HexGameControls
{
    public class MovableBorder : Border
    {
        public MovableBorder()
        {
            AttachedToVisualTree += OnAttachedToVisualTree;
            DetachedFromVisualTree += OnDetachedFromVisualTree;
        }

        Canvas? _canvas;

        private void OnAttachedToVisualTree(object? sender, VisualTreeAttachmentEventArgs e)
        {
            _canvas = Parent as Canvas;
            if (_canvas == null) return;
            _canvas.AddHandler(DragDrop.DragOverEvent, Canvas_DragOver);
        }

        private void OnDetachedFromVisualTree(object? sender, VisualTreeAttachmentEventArgs e)
        {
            if (_canvas == null) return;
            _canvas.RemoveHandler(DragDrop.DragOverEvent, Canvas_DragOver);
        }

        private void Canvas_DragOver(object? sender, DragEventArgs e)
        {
            var point = e.GetPosition(_canvas);
            Canvas.SetLeft(this, point.X + 20);
            Canvas.SetTop(this, point.Y + 20);
        }
    }
}
